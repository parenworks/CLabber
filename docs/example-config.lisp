;;; CLabber Example Configuration
;;; Copy this to ~/.config/clabber/config.lisp and edit as needed.

(:clabber-config
 :version 1
 :default-account "main"
 :roster-width 28
 :auto-reconnect t
 :auto-open-on-message nil
 :download-dir "/home/user/Downloads/CLabber/"
 :theme "tokyo-night"
 :accounts
 (;; Example account using pass(1) for password
  (:name "main"
   :jid "user@example.com"
   :password (:pass "xmpp/example.com")
   :port 5222
   :use-tls t
   :autoconnect t)

  ;; Example account using .authinfo or .authinfo.gpg
  ;; Add to ~/.authinfo: machine jabber.org login myuser password secret123
  (:name "jabber"
   :jid "myuser@jabber.org"
   :password :authinfo
   :port 5222
   :use-tls t
   :autoconnect nil)

  ;; Example account using systemd-creds
  (:name "work"
   :jid "employee@work.example.com"
   :password (:systemd-creds "/etc/credstore/xmpp-work.cred")
   :host "xmpp.work.example.com"
   :port 5222
   :use-tls t
   :autoconnect nil)

  ;; Example account using SASL EXTERNAL with client certificate
  (:name "secure"
   :jid "user@secure.example.com"
   :sasl :external
   :client-cert "/home/user/.config/clabber/certs/secure.pem"
   :port 5222
   :use-tls t
   :autoconnect nil)

  ;; Example account with plain text password (not recommended)
  (:name "test"
   :jid "testuser@localhost"
   :password "plaintext-password-not-recommended"
   :host "127.0.0.1"
   :port 5222
   :use-tls nil
   :autoconnect nil)))
