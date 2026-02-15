;;;; packages.lisp - Package definitions for CLabber
;;;; XMPP Chat Client - Pure ANSI Terminal UI

(defpackage #:clabber.ansi
  (:use #:cl)
  (:export ;; Color classes
           #:color
           #:indexed-color
           #:rgb-color
           #:named-color
           #:color-index
           #:color-red
           #:color-green
           #:color-blue
           #:color-name
           #:make-indexed-color
           #:make-rgb-color
           #:make-named-color
           #:lookup-color
           #:register-color
           #:emit-fg
           #:emit-bg
           ;; Style class
           #:text-style
           #:make-style
           #:style-fg
           #:style-bg
           #:style-bold-p
           #:style-dim-p
           #:style-italic-p
           #:style-underline-p
           #:style-inverse-p
           #:emit-style
           ;; Terminal class
           #:terminal
           #:terminal-stream
           #:terminal-width
           #:terminal-height
           #:term-cursor-to
           #:term-cursor-home
           #:term-cursor-hide
           #:term-cursor-show
           #:term-clear-screen
           #:term-clear-line
           #:term-clear-to-end
           #:term-reset
           #:term-write
           #:*terminal*
           ;; Convenience functions
           #:*escape*
           #:cursor-to
           #:cursor-home
           #:cursor-up
           #:cursor-down
           #:cursor-forward
           #:cursor-back
           #:cursor-hide
           #:cursor-show
           #:clear-screen
           #:clear-line
           #:clear-to-end
           #:begin-sync-update
           #:end-sync-update
           #:fg
           #:bg
           #:fg-rgb
           #:bg-rgb
           #:reset
           #:bold
           #:dim
           #:italic
           #:underline
           #:inverse
           #:color-code
           #:with-style))

(defpackage #:clabber.terminal
  (:use #:cl #:clabber.ansi)
  (:export ;; Key event class and accessors
           #:key-event
           #:make-key-event
           #:key-event-char
           #:key-event-code
           #:key-event-ctrl-p
           #:key-event-alt-p
           ;; Key constants
           #:+key-up+
           #:+key-down+
           #:+key-left+
           #:+key-right+
           #:+key-enter+
           #:+key-escape+
           #:+key-tab+
           #:+key-backspace+
           #:+key-delete+
           #:+key-home+
           #:+key-end+
           #:+key-page-up+
           #:+key-page-down+
           #:+key-mouse+
           ;; Terminal mode
           #:terminal-mode
           #:*terminal-mode*
           #:enable-raw-mode
           #:disable-raw-mode
           #:query-size
           #:terminal-size
           ;; Screen management
           #:enter-alternate-screen
           #:leave-alternate-screen
           #:enable-mouse-tracking
           #:disable-mouse-tracking
           #:with-raw-terminal
           #:setup-terminal
           #:restore-terminal
           ;; Input
           #:input-reader
           #:*input-reader*
           #:reader-open
           #:reader-close
           #:read-key-event
           #:read-key
           #:read-key-with-timeout
           #:close-tty-stream))

(defpackage #:clabber.theme
  (:use #:cl #:clabber.ansi)
  (:export #:base-theme
           #:tokyo-night-theme
           #:*current-theme*
           #:current-theme
           ;; Theme accessors
           #:theme-nick-colors
           #:theme-nick-color
           #:theme-level-color
           #:theme-bg
           #:theme-fg
           #:theme-border-active
           #:theme-border-inactive
           #:theme-unread-indicator
           #:theme-mention-indicator
           #:theme-omemo-lock
           #:theme-timestamp
           #:theme-join-color
           #:theme-part-color
           #:theme-error-color
           #:theme-system-color
           #:theme-presence-color
           #:theme-input-prompt
           #:theme-box-h
           #:theme-box-v
           #:theme-box-tl
           #:theme-box-tr
           #:theme-box-bl
           #:theme-box-br
           #:theme-box-t-down
           #:theme-box-t-up
           #:theme-box-t-right
           #:theme-box-t-left
           #:theme-box-cross))

(defpackage #:clabber.model
  (:use #:cl)
  (:export ;; Message
           #:message
           #:make-message
           #:message-text
           #:message-nick
           #:message-timestamp
           #:message-level
           #:message-highlight-p
           #:message-stanza-id
           #:message-edited-p
           ;; Buffer
           #:buffer
           #:make-buffer
           #:buffer-name
           #:buffer-display-name
           #:buffer-type
           #:buffer-messages
           #:buffer-unread-count
           #:buffer-mention-p
           #:buffer-participants
           #:buffer-scroll-offset
           #:buffer-omemo-p
           #:buffer-topic
           #:buffer-modes
           #:buffer-last-sent-id
           #:buffer-last-sent-text
           #:buffer-correcting-p
           #:buffer-add-message
           #:buffer-correct-message
           #:buffer-add-participant
           #:buffer-remove-participant
           #:buffer-participant-nicks
           ;; Participant helpers
           #:participant-nick
           #:participant-role
           #:role-prefix
           #:affiliation-to-role
           ;; Contact
           #:contact
           #:make-contact
           #:contact-jid
           #:contact-nick
           #:contact-presence
           #:contact-subscription
           ;; Utilities
           #:strip-muc-name))

(defpackage #:clabber.widgets
  (:use #:cl #:clabber.ansi #:clabber.theme #:clabber.model)
  (:export #:panel
           #:panel-x
           #:panel-y
           #:panel-width
           #:panel-height
           #:panel-title
           #:panel-active-p
           #:panel-render
           ;; Chat panel
           #:chat-panel
           #:chat-panel-buffer
           ;; Roster panel
           #:roster-panel
           #:roster-dm-buffers
           #:roster-muc-buffers
           #:roster-selected-index
           ;; Participants panel
           #:participants-panel
           #:participants-list
           #:participants-selected-index
           #:role-sort-key
           ;; Input bar
           #:input-bar
           #:input-bar-text
           #:input-bar-cursor-pos
           ;; Status bar
           #:status-bar
           #:status-bar-left
           #:status-bar-right
           ;; Buffer bar
           #:buffer-bar
           #:buffer-bar-buffers
           #:buffer-bar-active-index
           #:buffer-bar-rows-needed
           ;; Splash
           #:splash-screen
           #:render-splash
           #:splash-advance
           #:splash-error))

(defpackage #:clabber.layout
  (:use #:cl #:clabber.ansi #:clabber.widgets)
  (:export #:layout
           #:make-layout
           #:layout-compute
           #:layout-render-all
           #:layout-split-mode
           #:layout-roster-width
           #:layout-participants-width
           #:layout-input
           #:layout-status
           #:layout-roster
           #:layout-chat-a
           #:layout-chat-b
           #:layout-participants
           #:layout-buffer-bar))

(defpackage #:clabber.config
  (:use #:cl)
  (:export #:config
           #:make-config
           #:load-config
           #:save-config
           #:config-accounts
           ;; Account config
           #:account-config
           #:account-jid
           #:account-server
           #:account-port
           #:account-tls-p
           #:account-auth-method
           #:account-password
           #:account-client-cert
           #:account-autojoin
           #:account-autoconnect-p
           #:lookup-authinfo-password))

(defpackage #:clabber.xmpp
  (:use #:cl)
  (:export ;; XML element
           #:xml-element
           #:make-xml-element
           #:xml-name
           #:xml-namespace
           #:xml-attributes
           #:xml-children
           #:xml-text
           #:xml-attr
           #:xml-child
           #:xml-children-named
           #:xml-child-by-ns
           #:serialize-xml
           #:parse-full-element
           #:decode-xml-entities
           ;; XML namespaces
           #:+ns-stream+ #:+ns-client+ #:+ns-sasl+ #:+ns-tls+
           #:+ns-bind+ #:+ns-session+ #:+ns-roster+
           #:+ns-disco-info+ #:+ns-disco-items+
           #:+ns-muc+ #:+ns-muc-user+ #:+ns-pubsub+
           #:+ns-bookmarks+ #:+ns-bookmarks2+ #:+ns-private+
           #:+ns-carbons+ #:+ns-chatstates+ #:+ns-mam+
           ;; Stream
           #:xmpp-stream
           #:xmpp-stream-connect
           #:xmpp-stream-connect-tls
           #:xmpp-stream-disconnect
           #:xmpp-stream-send
           #:xmpp-stream-send-raw
           ;; Connection
           #:xmpp-connection
           #:xmpp-connect
           #:xmpp-disconnect
           #:xmpp-send
           #:xmpp-send-message
           #:xmpp-send-omemo-message
           #:xmpp-send-groupchat
           #:xmpp-send-correction
           #:xmpp-send-presence
           #:xmpp-send-chat-state
           #:xmpp-receive
           #:xmpp-join-muc
           #:xmpp-leave-muc
           #:xmpp-get-roster
           #:xmpp-get-bookmarks
           #:xmpp-enable-carbons
           #:xmpp-send-ping
           #:xmpp-disco-info
           #:xmpp-query-mam
           #:xmpp-accept-subscription
           #:xmpp-add-contact
           ;; PEP / OMEMO PubSub
           #:xmpp-pep-publish
           #:xmpp-pep-fetch
           #:xmpp-publish-omemo-devicelist
           #:xmpp-publish-omemo-bundle
           #:xmpp-fetch-omemo-devicelist
           #:xmpp-fetch-omemo-bundle
           #:xmpp-send-omemo-message
           #:conn-jid
           #:conn-bound-jid
           #:conn-connected-p
           #:conn-stream
           ;; Stanza types
           #:stanza
           #:stanza-id #:stanza-to #:stanza-from #:stanza-type #:stanza-xml
           #:message-stanza
           #:message-body #:message-subject #:message-delay #:message-chat-state #:message-omemo-encrypted
           #:presence-stanza
           #:presence-show #:presence-status
           #:iq-stanza
           #:iq-query
           #:make-message-stanza #:make-presence-stanza #:make-iq-stanza
           #:stanza-to-xml
           #:parse-stanza
           #:parse-bookmarks
           #:generate-id
           ;; JID utilities
           #:parse-jid
           #:bare-jid
           #:jid-resource
           ;; Debug
           #:debug-log
           #:open-debug-log
           #:close-debug-log
           #:current-timestamp))

(defpackage #:clabber.crypto
  (:use #:cl)
  (:export ;; Signal FFI
           #:signal-init
           #:signal-cleanup
           #:signal-generate-identity
           #:signal-generate-prekeys
           #:signal-generate-signed-prekey
           #:signal-session-encrypt
           #:signal-session-decrypt
           #:signal-build-session
           ;; OMEMO layer
           #:omemo-init
           #:omemo-encrypt-message
           #:omemo-decrypt-message
           #:omemo-enabled-p
           #:omemo-bundle-xml
           #:omemo-encrypt-payload
           #:omemo-decrypt-payload
           #:*omemo-device-id*
           #:*omemo-registration-id*
           #:*device-list-cache*
           #:cache-device-list
           #:get-cached-device-list
           #:base64-to-bytes
           #:bytes-to-base64
           #:signal-get-identity-public
           #:signal-has-identity-p
           #:signal-get-registration-id
           #:signal-has-session-p
           #:signal-build-session
           #:+omemo-ns-legacy+
           #:+omemo-devicelist-node-legacy+
           #:+omemo-bundles-node-legacy+
           #:+ciphertext-prekey-type+
           #:+ciphertext-signal-type+))

(defpackage #:clabber
  (:use #:cl
        #:clabber.ansi
        #:clabber.terminal
        #:clabber.theme
        #:clabber.model
        #:clabber.widgets
        #:clabber.layout
        #:clabber.config
        #:clabber.xmpp
        #:clabber.crypto)
  (:export #:main
           #:*version*))
