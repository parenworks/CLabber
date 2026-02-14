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
           #:buffer-add-message
           ;; Contact
           #:contact
           #:make-contact
           #:contact-jid
           #:contact-nick
           #:contact-presence
           #:contact-subscription))

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
           #:account-autoconnect-p))

(defpackage #:clabber.xmpp
  (:use #:cl)
  (:export #:xmpp-connection
           #:xmpp-connect
           #:xmpp-disconnect
           #:xmpp-send
           #:xmpp-send-message
           #:xmpp-join-muc
           #:xmpp-leave-muc
           #:xmpp-set-presence
           ;; Stanza types
           #:stanza
           #:message-stanza
           #:presence-stanza
           #:iq-stanza
           ;; Callbacks
           #:on-message
           #:on-presence
           #:on-iq
           #:on-connect
           #:on-disconnect))

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
           #:omemo-publish-bundle
           #:omemo-fetch-bundle
           #:omemo-enabled-p))

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
