/*
 * clabber-signal-helper.c
 * Thin C wrapper around libsignal-protocol-c providing:
 * - OpenSSL-based crypto provider (random, HMAC-SHA256, SHA512, AES)
 * - File-based session/prekey/identity stores
 * - High-level init/keygen/encrypt/decrypt API callable from CFFI
 *
 * Compile: gcc -shared -fPIC -o libclabber-signal.so clabber-signal-helper.c \
 *          -lsignal-protocol-c -lssl -lcrypto
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include <sys/stat.h>
#include <errno.h>
#include <time.h>

#include <openssl/rand.h>
#include <openssl/evp.h>
#include <openssl/sha.h>
#include <openssl/core_names.h>
#include <openssl/params.h>

#include <signal/signal_protocol.h>
#include <signal/signal_protocol_types.h>
#include <signal/key_helper.h>
#include <signal/session_builder.h>
#include <signal/session_cipher.h>
#include <signal/session_pre_key.h>
#include <signal/protocol.h>
#include <signal/curve.h>

/* ============================================================
 * Global state
 * ============================================================ */

static signal_context *g_context = NULL;
static signal_protocol_store_context *g_store_context = NULL;
static ratchet_identity_key_pair *g_identity_key_pair = NULL;
static uint32_t g_registration_id = 0;
static char g_store_path[8192] = {0};

/* ============================================================
 * Utility: file I/O for store persistence
 * ============================================================ */

static void ensure_dir(const char *path) {
    mkdir(path, 0700);
}

static char *make_path(const char *dir, const char *file) {
    static char buf[8192];
    snprintf(buf, sizeof(buf), "%s/%s", dir, file);
    return buf;
}

static char *make_session_path(const char *name, int device_id) {
    static char buf[8192];
    snprintf(buf, sizeof(buf), "%s/sessions/%s_%d.bin", g_store_path, name, device_id);
    return buf;
}

static char *make_prekey_path(uint32_t id) {
    static char buf[8192];
    snprintf(buf, sizeof(buf), "%s/prekeys/%u.bin", g_store_path, id);
    return buf;
}

static char *make_signed_prekey_path(uint32_t id) {
    static char buf[8192];
    snprintf(buf, sizeof(buf), "%s/signed_prekeys/%u.bin", g_store_path, id);
    return buf;
}

static char *make_identity_path(const char *name, int device_id) {
    static char buf[8192];
    snprintf(buf, sizeof(buf), "%s/identities/%s_%d.bin", g_store_path, name, device_id);
    return buf;
}

static int file_write(const char *path, const uint8_t *data, size_t len) {
    FILE *f = fopen(path, "wb");
    if (!f) return -1;
    size_t written = fwrite(data, 1, len, f);
    fclose(f);
    return (written == len) ? 0 : -1;
}

static int file_read(const char *path, uint8_t **data, size_t *len) {
    FILE *f = fopen(path, "rb");
    if (!f) return -1;
    fseek(f, 0, SEEK_END);
    long size = ftell(f);
    fseek(f, 0, SEEK_SET);
    if (size <= 0) { fclose(f); return -1; }
    *data = malloc(size);
    if (!*data) { fclose(f); return -1; }
    *len = fread(*data, 1, size, f);
    fclose(f);
    return 0;
}

static int file_exists(const char *path) {
    struct stat st;
    return stat(path, &st) == 0;
}

static int file_remove(const char *path) {
    return remove(path);
}

/* ============================================================
 * OpenSSL crypto provider callbacks
 * ============================================================ */

static int openssl_random(uint8_t *data, size_t len, void *user_data) {
    (void)user_data;
    if (RAND_bytes(data, (int)len) != 1) return SG_ERR_UNKNOWN;
    return 0;
}

static int openssl_hmac_sha256_init(void **ctx, const uint8_t *key, size_t key_len, void *user_data) {
    (void)user_data;
    EVP_MAC *mac = EVP_MAC_fetch(NULL, "HMAC", NULL);
    if (!mac) return SG_ERR_UNKNOWN;
    EVP_MAC_CTX *mac_ctx = EVP_MAC_CTX_new(mac);
    EVP_MAC_free(mac);
    if (!mac_ctx) return SG_ERR_NOMEM;
    OSSL_PARAM params[] = {
        OSSL_PARAM_construct_utf8_string(OSSL_MAC_PARAM_DIGEST, "SHA256", 0),
        OSSL_PARAM_construct_end()
    };
    if (EVP_MAC_init(mac_ctx, key, key_len, params) != 1) {
        EVP_MAC_CTX_free(mac_ctx);
        return SG_ERR_UNKNOWN;
    }
    *ctx = mac_ctx;
    return 0;
}

static int openssl_hmac_sha256_update(void *ctx, const uint8_t *data, size_t data_len, void *user_data) {
    (void)user_data;
    if (EVP_MAC_update((EVP_MAC_CTX *)ctx, data, data_len) != 1) return SG_ERR_UNKNOWN;
    return 0;
}

static int openssl_hmac_sha256_final(void *ctx, signal_buffer **output, void *user_data) {
    (void)user_data;
    unsigned char md[EVP_MAX_MD_SIZE];
    size_t md_len = 0;
    if (EVP_MAC_final((EVP_MAC_CTX *)ctx, md, &md_len, sizeof(md)) != 1) return SG_ERR_UNKNOWN;
    *output = signal_buffer_create(md, md_len);
    return *output ? 0 : SG_ERR_NOMEM;
}

static void openssl_hmac_sha256_cleanup(void *ctx, void *user_data) {
    (void)user_data;
    if (ctx) EVP_MAC_CTX_free((EVP_MAC_CTX *)ctx);
}

static int openssl_sha512_init(void **ctx, void *user_data) {
    (void)user_data;
    EVP_MD_CTX *md_ctx = EVP_MD_CTX_new();
    if (!md_ctx) return SG_ERR_NOMEM;
    if (EVP_DigestInit_ex(md_ctx, EVP_sha512(), NULL) != 1) {
        EVP_MD_CTX_free(md_ctx);
        return SG_ERR_UNKNOWN;
    }
    *ctx = md_ctx;
    return 0;
}

static int openssl_sha512_update(void *ctx, const uint8_t *data, size_t data_len, void *user_data) {
    (void)user_data;
    if (EVP_DigestUpdate((EVP_MD_CTX *)ctx, data, data_len) != 1) return SG_ERR_UNKNOWN;
    return 0;
}

static int openssl_sha512_final(void *ctx, signal_buffer **output, void *user_data) {
    (void)user_data;
    unsigned char md[EVP_MAX_MD_SIZE];
    unsigned int md_len = 0;
    if (EVP_DigestFinal_ex((EVP_MD_CTX *)ctx, md, &md_len) != 1) return SG_ERR_UNKNOWN;
    /* Reset for reuse */
    EVP_DigestInit_ex((EVP_MD_CTX *)ctx, EVP_sha512(), NULL);
    *output = signal_buffer_create(md, md_len);
    return *output ? 0 : SG_ERR_NOMEM;
}

static void openssl_sha512_cleanup(void *ctx, void *user_data) {
    (void)user_data;
    if (ctx) EVP_MD_CTX_free((EVP_MD_CTX *)ctx);
}

static int openssl_encrypt(signal_buffer **output, int cipher,
        const uint8_t *key, size_t key_len,
        const uint8_t *iv, size_t iv_len,
        const uint8_t *plaintext, size_t plaintext_len,
        void *user_data) {
    (void)user_data; (void)iv_len;
    const EVP_CIPHER *evp_cipher = NULL;
    int do_padding = 0;

    if (cipher == SG_CIPHER_AES_CBC_PKCS5) {
        if (key_len == 32) evp_cipher = EVP_aes_256_cbc();
        else if (key_len == 16) evp_cipher = EVP_aes_128_cbc();
        else return SG_ERR_UNKNOWN;
        do_padding = 1;
    } else if (cipher == SG_CIPHER_AES_CTR_NOPADDING) {
        if (key_len == 32) evp_cipher = EVP_aes_256_ctr();
        else if (key_len == 16) evp_cipher = EVP_aes_128_ctr();
        else return SG_ERR_UNKNOWN;
        do_padding = 0;
    } else {
        return SG_ERR_UNKNOWN;
    }

    EVP_CIPHER_CTX *ctx = EVP_CIPHER_CTX_new();
    if (!ctx) return SG_ERR_NOMEM;

    EVP_CIPHER_CTX_set_padding(ctx, do_padding);
    if (EVP_EncryptInit_ex(ctx, evp_cipher, NULL, key, iv) != 1) {
        EVP_CIPHER_CTX_free(ctx);
        return SG_ERR_UNKNOWN;
    }

    uint8_t *out_buf = malloc(plaintext_len + EVP_CIPHER_block_size(evp_cipher));
    if (!out_buf) { EVP_CIPHER_CTX_free(ctx); return SG_ERR_NOMEM; }

    int out_len = 0, final_len = 0;
    if (EVP_EncryptUpdate(ctx, out_buf, &out_len, plaintext, (int)plaintext_len) != 1) {
        free(out_buf); EVP_CIPHER_CTX_free(ctx); return SG_ERR_UNKNOWN;
    }
    if (EVP_EncryptFinal_ex(ctx, out_buf + out_len, &final_len) != 1) {
        free(out_buf); EVP_CIPHER_CTX_free(ctx); return SG_ERR_UNKNOWN;
    }

    *output = signal_buffer_create(out_buf, out_len + final_len);
    free(out_buf);
    EVP_CIPHER_CTX_free(ctx);
    return *output ? 0 : SG_ERR_NOMEM;
}

static int openssl_decrypt(signal_buffer **output, int cipher,
        const uint8_t *key, size_t key_len,
        const uint8_t *iv, size_t iv_len,
        const uint8_t *ciphertext, size_t ciphertext_len,
        void *user_data) {
    (void)user_data; (void)iv_len;
    const EVP_CIPHER *evp_cipher = NULL;
    int do_padding = 0;

    if (cipher == SG_CIPHER_AES_CBC_PKCS5) {
        if (key_len == 32) evp_cipher = EVP_aes_256_cbc();
        else if (key_len == 16) evp_cipher = EVP_aes_128_cbc();
        else return SG_ERR_UNKNOWN;
        do_padding = 1;
    } else if (cipher == SG_CIPHER_AES_CTR_NOPADDING) {
        if (key_len == 32) evp_cipher = EVP_aes_256_ctr();
        else if (key_len == 16) evp_cipher = EVP_aes_128_ctr();
        else return SG_ERR_UNKNOWN;
        do_padding = 0;
    } else {
        return SG_ERR_UNKNOWN;
    }

    EVP_CIPHER_CTX *ctx = EVP_CIPHER_CTX_new();
    if (!ctx) return SG_ERR_NOMEM;

    EVP_CIPHER_CTX_set_padding(ctx, do_padding);
    if (EVP_DecryptInit_ex(ctx, evp_cipher, NULL, key, iv) != 1) {
        EVP_CIPHER_CTX_free(ctx);
        return SG_ERR_UNKNOWN;
    }

    uint8_t *out_buf = malloc(ciphertext_len + EVP_CIPHER_block_size(evp_cipher));
    if (!out_buf) { EVP_CIPHER_CTX_free(ctx); return SG_ERR_NOMEM; }

    int out_len = 0, final_len = 0;
    if (EVP_DecryptUpdate(ctx, out_buf, &out_len, ciphertext, (int)ciphertext_len) != 1) {
        free(out_buf); EVP_CIPHER_CTX_free(ctx); return SG_ERR_UNKNOWN;
    }
    if (EVP_DecryptFinal_ex(ctx, out_buf + out_len, &final_len) != 1) {
        free(out_buf); EVP_CIPHER_CTX_free(ctx); return SG_ERR_UNKNOWN;
    }

    *output = signal_buffer_create(out_buf, out_len + final_len);
    free(out_buf);
    EVP_CIPHER_CTX_free(ctx);
    return *output ? 0 : SG_ERR_NOMEM;
}

/* ============================================================
 * Session store callbacks (file-based)
 * ============================================================ */

static int ss_load_session(signal_buffer **record, signal_buffer **user_record,
        const signal_protocol_address *address, void *user_data) {
    (void)user_data; (void)user_record;
    char *path = make_session_path(address->name, address->device_id);
    uint8_t *data = NULL; size_t len = 0;
    if (file_read(path, &data, &len) != 0) return 0;
    *record = signal_buffer_create(data, len);
    free(data);
    return *record ? 1 : 0;
}

static int ss_get_sub_device_sessions(signal_int_list **sessions,
        const char *name, size_t name_len, void *user_data) {
    (void)user_data; (void)name; (void)name_len;
    *sessions = signal_int_list_alloc();
    return 0;
}

static int ss_store_session(const signal_protocol_address *address,
        uint8_t *record, size_t record_len,
        uint8_t *user_record, size_t user_record_len, void *user_data) {
    (void)user_data; (void)user_record; (void)user_record_len;
    char sessions_dir[8192];
    snprintf(sessions_dir, sizeof(sessions_dir), "%s/sessions", g_store_path);
    ensure_dir(sessions_dir);
    char *path = make_session_path(address->name, address->device_id);
    return file_write(path, record, record_len);
}

static int ss_contains_session(const signal_protocol_address *address, void *user_data) {
    (void)user_data;
    return file_exists(make_session_path(address->name, address->device_id));
}

static int ss_delete_session(const signal_protocol_address *address, void *user_data) {
    (void)user_data;
    return file_remove(make_session_path(address->name, address->device_id)) == 0 ? 1 : 0;
}

static int ss_delete_all_sessions(const char *name, size_t name_len, void *user_data) {
    (void)user_data; (void)name; (void)name_len;
    return 0;
}

static void ss_destroy(void *user_data) { (void)user_data; }

/* ============================================================
 * Pre-key store callbacks (file-based)
 * ============================================================ */

static int pks_load(signal_buffer **record, uint32_t pre_key_id, void *user_data) {
    (void)user_data;
    char *path = make_prekey_path(pre_key_id);
    uint8_t *data = NULL; size_t len = 0;
    if (file_read(path, &data, &len) != 0) return SG_ERR_INVALID_KEY_ID;
    *record = signal_buffer_create(data, len);
    free(data);
    return *record ? SG_SUCCESS : SG_ERR_NOMEM;
}

static int pks_store(uint32_t pre_key_id, uint8_t *record, size_t record_len, void *user_data) {
    (void)user_data;
    char prekeys_dir[8192];
    snprintf(prekeys_dir, sizeof(prekeys_dir), "%s/prekeys", g_store_path);
    ensure_dir(prekeys_dir);
    return file_write(make_prekey_path(pre_key_id), record, record_len);
}

static int pks_contains(uint32_t pre_key_id, void *user_data) {
    (void)user_data;
    return file_exists(make_prekey_path(pre_key_id));
}

static int pks_remove(uint32_t pre_key_id, void *user_data) {
    (void)user_data;
    file_remove(make_prekey_path(pre_key_id));
    return 0;
}

static void pks_destroy(void *user_data) { (void)user_data; }

/* ============================================================
 * Signed pre-key store callbacks (file-based)
 * ============================================================ */

static int spks_load(signal_buffer **record, uint32_t signed_pre_key_id, void *user_data) {
    (void)user_data;
    char *path = make_signed_prekey_path(signed_pre_key_id);
    uint8_t *data = NULL; size_t len = 0;
    if (file_read(path, &data, &len) != 0) return SG_ERR_INVALID_KEY_ID;
    *record = signal_buffer_create(data, len);
    free(data);
    return *record ? SG_SUCCESS : SG_ERR_NOMEM;
}

static int spks_store(uint32_t signed_pre_key_id, uint8_t *record, size_t record_len, void *user_data) {
    (void)user_data;
    char dir[8192];
    snprintf(dir, sizeof(dir), "%s/signed_prekeys", g_store_path);
    ensure_dir(dir);
    return file_write(make_signed_prekey_path(signed_pre_key_id), record, record_len);
}

static int spks_contains(uint32_t signed_pre_key_id, void *user_data) {
    (void)user_data;
    return file_exists(make_signed_prekey_path(signed_pre_key_id));
}

static int spks_remove(uint32_t signed_pre_key_id, void *user_data) {
    (void)user_data;
    file_remove(make_signed_prekey_path(signed_pre_key_id));
    return 0;
}

static void spks_destroy(void *user_data) { (void)user_data; }

/* ============================================================
 * Identity key store callbacks (file-based)
 * ============================================================ */

static int iks_get_identity_key_pair(signal_buffer **public_data, signal_buffer **private_data, void *user_data) {
    (void)user_data;
    if (!g_identity_key_pair) return SG_ERR_UNKNOWN;

    ec_public_key *pub = ratchet_identity_key_pair_get_public(g_identity_key_pair);
    ec_private_key *priv = ratchet_identity_key_pair_get_private(g_identity_key_pair);

    int result = ec_public_key_serialize(public_data, pub);
    if (result < 0) return result;

    result = ec_private_key_serialize(private_data, priv);
    if (result < 0) { signal_buffer_free(*public_data); return result; }

    return 0;
}

static int iks_get_local_registration_id(void *user_data, uint32_t *registration_id) {
    (void)user_data;
    *registration_id = g_registration_id;
    return 0;
}

static int iks_save_identity(const signal_protocol_address *address,
        uint8_t *key_data, size_t key_len, void *user_data) {
    (void)user_data;
    char dir[8192];
    snprintf(dir, sizeof(dir), "%s/identities", g_store_path);
    ensure_dir(dir);
    if (key_data && key_len > 0) {
        return file_write(make_identity_path(address->name, address->device_id), key_data, key_len);
    }
    return 0;
}

static int iks_is_trusted_identity(const signal_protocol_address *address,
        uint8_t *key_data, size_t key_len, void *user_data) {
    (void)user_data;
    /* Trust on first use */
    char *path = make_identity_path(address->name, address->device_id);
    if (!file_exists(path)) return 1; /* No existing key = trusted */

    uint8_t *stored = NULL; size_t stored_len = 0;
    if (file_read(path, &stored, &stored_len) != 0) return 1;

    int trusted = (stored_len == key_len && memcmp(stored, key_data, key_len) == 0) ? 1 : 0;
    free(stored);
    return trusted;
}

static void iks_destroy(void *user_data) { (void)user_data; }

/* ============================================================
 * Sender key store (stub - not used for OMEMO 1:1)
 * ============================================================ */

static int sks_store(const signal_protocol_sender_key_name *name,
        uint8_t *record, size_t record_len,
        uint8_t *user_record, size_t user_record_len, void *user_data) {
    (void)name; (void)record; (void)record_len;
    (void)user_record; (void)user_record_len; (void)user_data;
    return 0;
}

static int sks_load(signal_buffer **record, signal_buffer **user_record,
        const signal_protocol_sender_key_name *name, void *user_data) {
    (void)record; (void)user_record; (void)name; (void)user_data;
    return 0;
}

static void sks_destroy(void *user_data) { (void)user_data; }

/* ============================================================
 * Public API
 * ============================================================ */

int clabber_signal_init(const char *store_path) {
    int result;

    strncpy(g_store_path, store_path, sizeof(g_store_path) - 1);
    ensure_dir(g_store_path);

    /* Create global context */
    result = signal_context_create(&g_context, NULL);
    if (result != 0) return result;

    /* Set up OpenSSL crypto provider */
    signal_crypto_provider crypto_provider = {
        .random_func = openssl_random,
        .hmac_sha256_init_func = openssl_hmac_sha256_init,
        .hmac_sha256_update_func = openssl_hmac_sha256_update,
        .hmac_sha256_final_func = openssl_hmac_sha256_final,
        .hmac_sha256_cleanup_func = openssl_hmac_sha256_cleanup,
        .sha512_digest_init_func = openssl_sha512_init,
        .sha512_digest_update_func = openssl_sha512_update,
        .sha512_digest_final_func = openssl_sha512_final,
        .sha512_digest_cleanup_func = openssl_sha512_cleanup,
        .encrypt_func = openssl_encrypt,
        .decrypt_func = openssl_decrypt,
        .user_data = NULL
    };
    result = signal_context_set_crypto_provider(g_context, &crypto_provider);
    if (result != 0) return result;

    /* Create store context */
    result = signal_protocol_store_context_create(&g_store_context, g_context);
    if (result != 0) return result;

    /* Set up session store */
    signal_protocol_session_store session_store = {
        .load_session_func = ss_load_session,
        .get_sub_device_sessions_func = ss_get_sub_device_sessions,
        .store_session_func = ss_store_session,
        .contains_session_func = ss_contains_session,
        .delete_session_func = ss_delete_session,
        .delete_all_sessions_func = ss_delete_all_sessions,
        .destroy_func = ss_destroy,
        .user_data = NULL
    };
    result = signal_protocol_store_context_set_session_store(g_store_context, &session_store);
    if (result != 0) return result;

    /* Set up pre-key store */
    signal_protocol_pre_key_store pre_key_store = {
        .load_pre_key = pks_load,
        .store_pre_key = pks_store,
        .contains_pre_key = pks_contains,
        .remove_pre_key = pks_remove,
        .destroy_func = pks_destroy,
        .user_data = NULL
    };
    result = signal_protocol_store_context_set_pre_key_store(g_store_context, &pre_key_store);
    if (result != 0) return result;

    /* Set up signed pre-key store */
    signal_protocol_signed_pre_key_store signed_pre_key_store = {
        .load_signed_pre_key = spks_load,
        .store_signed_pre_key = spks_store,
        .contains_signed_pre_key = spks_contains,
        .remove_signed_pre_key = spks_remove,
        .destroy_func = spks_destroy,
        .user_data = NULL
    };
    result = signal_protocol_store_context_set_signed_pre_key_store(g_store_context, &signed_pre_key_store);
    if (result != 0) return result;

    /* Set up identity key store */
    signal_protocol_identity_key_store identity_store = {
        .get_identity_key_pair = iks_get_identity_key_pair,
        .get_local_registration_id = iks_get_local_registration_id,
        .save_identity = iks_save_identity,
        .is_trusted_identity = iks_is_trusted_identity,
        .destroy_func = iks_destroy,
        .user_data = NULL
    };
    result = signal_protocol_store_context_set_identity_key_store(g_store_context, &identity_store);
    if (result != 0) return result;

    /* Set up sender key store (stub) */
    signal_protocol_sender_key_store sender_key_store = {
        .store_sender_key = sks_store,
        .load_sender_key = sks_load,
        .destroy_func = sks_destroy,
        .user_data = NULL
    };
    result = signal_protocol_store_context_set_sender_key_store(g_store_context, &sender_key_store);
    if (result != 0) return result;

    /* Try to load existing identity from disk */
    char *id_path = make_path(g_store_path, "identity.bin");
    if (file_exists(id_path)) {
        uint8_t *data = NULL; size_t len = 0;
        if (file_read(id_path, &data, &len) == 0 && len >= 4) {
            /* First 4 bytes: registration ID (little-endian) */
            g_registration_id = data[0] | (data[1] << 8) | (data[2] << 16) | (data[3] << 24);
            /* Rest: serialized identity key pair (pub_len(2) + pub + priv_len(2) + priv) */
            if (len > 6) {
                uint16_t pub_len = data[4] | (data[5] << 8);
                if (len >= (size_t)(8 + pub_len)) {
                    uint8_t *pub_data = data + 6;
                    uint16_t priv_len = data[6 + pub_len] | (data[7 + pub_len] << 8);
                    uint8_t *priv_data = data + 8 + pub_len;

                    if (len >= (size_t)(8 + pub_len + priv_len)) {
                        ec_public_key *pub_key = NULL;
                        ec_private_key *priv_key = NULL;
                        result = curve_decode_point(&pub_key, pub_data, pub_len, g_context);
                        if (result == 0) {
                            result = curve_decode_private_point(&priv_key, priv_data, priv_len, g_context);
                        }
                        if (result == 0 && pub_key && priv_key) {
                            result = ratchet_identity_key_pair_create(&g_identity_key_pair, pub_key, priv_key);
                            if (result == 0) {
                                fprintf(stderr, "clabber-signal: loaded identity, reg_id=%u\n", g_registration_id);
                            }
                        }
                        if (pub_key) SIGNAL_UNREF(pub_key);
                        if (priv_key) SIGNAL_UNREF(priv_key);
                    }
                }
            }
            free(data);
        }
    }

    return 0;
}

void clabber_signal_cleanup(void) {
    if (g_identity_key_pair) {
        SIGNAL_UNREF(g_identity_key_pair);
        g_identity_key_pair = NULL;
    }
    if (g_store_context) {
        signal_protocol_store_context_destroy(g_store_context);
        g_store_context = NULL;
    }
    if (g_context) {
        signal_context_destroy(g_context);
        g_context = NULL;
    }
}

signal_context *clabber_signal_get_context(void) {
    return g_context;
}

signal_protocol_store_context *clabber_signal_get_store(void) {
    return g_store_context;
}

int clabber_signal_generate_identity(
        uint32_t *registration_id_out,
        uint8_t **identity_pub_out, size_t *identity_pub_len_out,
        uint8_t **identity_priv_out, size_t *identity_priv_len_out) {
    int result;

    /* Generate identity key pair */
    if (g_identity_key_pair) SIGNAL_UNREF(g_identity_key_pair);
    result = signal_protocol_key_helper_generate_identity_key_pair(&g_identity_key_pair, g_context);
    if (result != 0) return result;

    /* Generate registration ID */
    result = signal_protocol_key_helper_generate_registration_id(&g_registration_id, 0, g_context);
    if (result != 0) return result;

    *registration_id_out = g_registration_id;

    /* Serialize public key */
    signal_buffer *pub_buf = NULL;
    ec_public_key *pub = ratchet_identity_key_pair_get_public(g_identity_key_pair);
    result = ec_public_key_serialize(&pub_buf, pub);
    if (result != 0) return result;

    *identity_pub_len_out = signal_buffer_len(pub_buf);
    *identity_pub_out = malloc(*identity_pub_len_out);
    memcpy(*identity_pub_out, signal_buffer_data(pub_buf), *identity_pub_len_out);
    signal_buffer_free(pub_buf);

    /* Serialize private key */
    signal_buffer *priv_buf = NULL;
    ec_private_key *priv = ratchet_identity_key_pair_get_private(g_identity_key_pair);
    result = ec_private_key_serialize(&priv_buf, priv);
    if (result != 0) { free(*identity_pub_out); return result; }

    *identity_priv_len_out = signal_buffer_len(priv_buf);
    *identity_priv_out = malloc(*identity_priv_len_out);
    memcpy(*identity_priv_out, signal_buffer_data(priv_buf), *identity_priv_len_out);
    signal_buffer_free(priv_buf);

    /* Save identity to disk */
    char *id_path = make_path(g_store_path, "identity.bin");
    size_t total = 4 + 2 + *identity_pub_len_out + 2 + *identity_priv_len_out;
    uint8_t *save_data = malloc(total);
    if (save_data) {
        save_data[0] = g_registration_id & 0xFF;
        save_data[1] = (g_registration_id >> 8) & 0xFF;
        save_data[2] = (g_registration_id >> 16) & 0xFF;
        save_data[3] = (g_registration_id >> 24) & 0xFF;
        save_data[4] = *identity_pub_len_out & 0xFF;
        save_data[5] = (*identity_pub_len_out >> 8) & 0xFF;
        memcpy(save_data + 6, *identity_pub_out, *identity_pub_len_out);
        size_t off = 6 + *identity_pub_len_out;
        save_data[off] = *identity_priv_len_out & 0xFF;
        save_data[off+1] = (*identity_priv_len_out >> 8) & 0xFF;
        memcpy(save_data + off + 2, *identity_priv_out, *identity_priv_len_out);
        file_write(id_path, save_data, total);
        free(save_data);
    }

    return 0;
}

int clabber_signal_generate_prekeys(unsigned int start, unsigned int count) {
    signal_protocol_key_helper_pre_key_list_node *head = NULL;
    int result = signal_protocol_key_helper_generate_pre_keys(&head, start, count, g_context);
    if (result != 0) return result;

    /* Store each pre-key */
    signal_protocol_key_helper_pre_key_list_node *node = head;
    while (node) {
        session_pre_key *pre_key = signal_protocol_key_helper_key_list_element(node);
        result = signal_protocol_pre_key_store_key(g_store_context, pre_key);
        if (result != 0) break;
        node = signal_protocol_key_helper_key_list_next(node);
    }

    signal_protocol_key_helper_key_list_free(head);
    return result;
}

int clabber_signal_generate_signed_prekey(uint32_t signed_prekey_id) {
    if (!g_identity_key_pair) return SG_ERR_UNKNOWN;

    session_signed_pre_key *signed_pre_key = NULL;
    uint64_t timestamp = (uint64_t)time(NULL) * 1000;
    int result = signal_protocol_key_helper_generate_signed_pre_key(
        &signed_pre_key, g_identity_key_pair, signed_prekey_id, timestamp, g_context);
    if (result != 0) return result;

    result = signal_protocol_signed_pre_key_store_key(g_store_context, signed_pre_key);
    SIGNAL_UNREF(signed_pre_key);
    return result;
}

int clabber_signal_get_prekey_public(uint32_t prekey_id,
        uint8_t **pub_out, size_t *pub_len_out) {
    session_pre_key *pre_key = NULL;
    int result = signal_protocol_pre_key_load_key(g_store_context, &pre_key, prekey_id);
    if (result != 0) return result;

    ec_key_pair *kp = session_pre_key_get_key_pair(pre_key);
    ec_public_key *pub = ec_key_pair_get_public(kp);

    signal_buffer *buf = NULL;
    result = ec_public_key_serialize(&buf, pub);
    SIGNAL_UNREF(pre_key);
    if (result != 0) return result;

    *pub_len_out = signal_buffer_len(buf);
    *pub_out = malloc(*pub_len_out);
    memcpy(*pub_out, signal_buffer_data(buf), *pub_len_out);
    signal_buffer_free(buf);
    return 0;
}

int clabber_signal_get_signed_prekey_public(uint32_t signed_prekey_id,
        uint8_t **pub_out, size_t *pub_len_out,
        uint8_t **sig_out, size_t *sig_len_out) {
    session_signed_pre_key *spk = NULL;
    int result = signal_protocol_signed_pre_key_load_key(g_store_context, &spk, signed_prekey_id);
    if (result != 0) return result;

    ec_key_pair *kp = session_signed_pre_key_get_key_pair(spk);
    ec_public_key *pub = ec_key_pair_get_public(kp);

    signal_buffer *buf = NULL;
    result = ec_public_key_serialize(&buf, pub);
    if (result != 0) { SIGNAL_UNREF(spk); return result; }

    *pub_len_out = signal_buffer_len(buf);
    *pub_out = malloc(*pub_len_out);
    memcpy(*pub_out, signal_buffer_data(buf), *pub_len_out);
    signal_buffer_free(buf);

    /* Signature */
    const uint8_t *sig = session_signed_pre_key_get_signature(spk);
    *sig_len_out = session_signed_pre_key_get_signature_len(spk);
    *sig_out = malloc(*sig_len_out);
    memcpy(*sig_out, sig, *sig_len_out);

    SIGNAL_UNREF(spk);
    return 0;
}

int clabber_signal_get_identity_public(uint8_t **pub_out, size_t *pub_len_out) {
    if (!g_identity_key_pair) return SG_ERR_UNKNOWN;

    ec_public_key *pub = ratchet_identity_key_pair_get_public(g_identity_key_pair);
    signal_buffer *buf = NULL;
    int result = ec_public_key_serialize(&buf, pub);
    if (result != 0) return result;

    *pub_len_out = signal_buffer_len(buf);
    *pub_out = malloc(*pub_len_out);
    memcpy(*pub_out, signal_buffer_data(buf), *pub_len_out);
    signal_buffer_free(buf);
    return 0;
}

int clabber_signal_build_session(const char *name, int32_t device_id,
        uint32_t registration_id,
        uint32_t prekey_id, const uint8_t *prekey_public, size_t prekey_public_len,
        uint32_t signed_prekey_id, const uint8_t *signed_prekey_public, size_t signed_prekey_public_len,
        const uint8_t *signed_prekey_signature, size_t signed_prekey_signature_len,
        const uint8_t *identity_key, size_t identity_key_len) {
    int result;

    /* Decode keys */
    ec_public_key *pk_pub = NULL, *spk_pub = NULL, *ik_pub = NULL;

    result = curve_decode_point(&pk_pub, prekey_public, prekey_public_len, g_context);
    if (result != 0) return result;

    result = curve_decode_point(&spk_pub, signed_prekey_public, signed_prekey_public_len, g_context);
    if (result != 0) { SIGNAL_UNREF(pk_pub); return result; }

    result = curve_decode_point(&ik_pub, identity_key, identity_key_len, g_context);
    if (result != 0) { SIGNAL_UNREF(pk_pub); SIGNAL_UNREF(spk_pub); return result; }

    /* Create bundle */
    session_pre_key_bundle *bundle = NULL;
    result = session_pre_key_bundle_create(&bundle,
        registration_id, device_id,
        prekey_id, pk_pub,
        signed_prekey_id, spk_pub,
        signed_prekey_signature, signed_prekey_signature_len,
        ik_pub);

    SIGNAL_UNREF(pk_pub);
    SIGNAL_UNREF(spk_pub);
    SIGNAL_UNREF(ik_pub);

    if (result != 0) return result;

    /* Create session builder */
    signal_protocol_address address = {
        .name = name,
        .name_len = strlen(name),
        .device_id = device_id
    };

    session_builder *builder = NULL;
    result = session_builder_create(&builder, g_store_context, &address, g_context);
    if (result != 0) { SIGNAL_UNREF(bundle); return result; }

    /* Process bundle to establish session */
    result = session_builder_process_pre_key_bundle(builder, bundle);

    session_builder_free(builder);
    SIGNAL_UNREF(bundle);
    return result;
}

int clabber_signal_encrypt(const char *name, int32_t device_id,
        const uint8_t *plaintext, size_t plaintext_len,
        uint8_t **ciphertext_out, size_t *ciphertext_len_out,
        int *msg_type_out) {
    signal_protocol_address address = {
        .name = name,
        .name_len = strlen(name),
        .device_id = device_id
    };

    session_cipher *cipher = NULL;
    int result = session_cipher_create(&cipher, g_store_context, &address, g_context);
    if (result != 0) return result;

    ciphertext_message *encrypted = NULL;
    result = session_cipher_encrypt(cipher, plaintext, plaintext_len, &encrypted);
    if (result != 0) { session_cipher_free(cipher); return result; }

    *msg_type_out = ciphertext_message_get_type(encrypted);
    signal_buffer *serialized = ciphertext_message_get_serialized(encrypted);

    *ciphertext_len_out = signal_buffer_len(serialized);
    *ciphertext_out = malloc(*ciphertext_len_out);
    memcpy(*ciphertext_out, signal_buffer_data(serialized), *ciphertext_len_out);

    SIGNAL_UNREF(encrypted);
    session_cipher_free(cipher);
    return 0;
}

int clabber_signal_decrypt(const char *name, int32_t device_id,
        const uint8_t *ciphertext, size_t ciphertext_len,
        int is_prekey,
        uint8_t **plaintext_out, size_t *plaintext_len_out) {
    signal_protocol_address address = {
        .name = name,
        .name_len = strlen(name),
        .device_id = device_id
    };

    session_cipher *cipher = NULL;
    int result = session_cipher_create(&cipher, g_store_context, &address, g_context);
    if (result != 0) return result;

    signal_buffer *plaintext = NULL;

    if (is_prekey) {
        pre_key_signal_message *msg = NULL;
        result = pre_key_signal_message_deserialize(&msg, ciphertext, ciphertext_len, g_context);
        if (result != 0) { session_cipher_free(cipher); return result; }

        result = session_cipher_decrypt_pre_key_signal_message(cipher, msg, NULL, &plaintext);
        SIGNAL_UNREF(msg);
    } else {
        signal_message *msg = NULL;
        result = signal_message_deserialize(&msg, ciphertext, ciphertext_len, g_context);
        if (result != 0) { session_cipher_free(cipher); return result; }

        result = session_cipher_decrypt_signal_message(cipher, msg, NULL, &plaintext);
        SIGNAL_UNREF(msg);
    }

    session_cipher_free(cipher);

    if (result != 0) return result;

    *plaintext_len_out = signal_buffer_len(plaintext);
    *plaintext_out = malloc(*plaintext_len_out);
    memcpy(*plaintext_out, signal_buffer_data(plaintext), *plaintext_len_out);
    signal_buffer_free(plaintext);
    return 0;
}

int clabber_signal_has_identity(void) {
    return (g_identity_key_pair != NULL && g_registration_id != 0) ? 1 : 0;
}

uint32_t clabber_signal_get_registration_id(void) {
    return g_registration_id;
}

int clabber_signal_has_session(const char *name, int32_t device_id) {
    signal_protocol_address address = {
        .name = name,
        .name_len = strlen(name),
        .device_id = device_id
    };
    return signal_protocol_session_contains_session(g_store_context, &address);
}
