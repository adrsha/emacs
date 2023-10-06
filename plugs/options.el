;;
;; ░█████╗░██████╗░████████╗██╗░█████╗░███╗░░██╗░██████╗
;; ██╔══██╗██╔══██╗╚══██╔══╝██║██╔══██╗████╗░██║██╔════╝
;; ██║░░██║██████╔╝░░░██║░░░██║██║░░██║██╔██╗██║╚█████╗░
;; ██║░░██║██╔═══╝░░░░██║░░░██║██║░░██║██║╚████║░╚═══██╗
;; ╚█████╔╝██║░░░░░░░░██║░░░██║╚█████╔╝██║░╚███║██████╔╝
;; ░╚════╝░╚═╝░░░░░░░░╚═╝░░░╚═╝░╚════╝░╚═╝░░╚══╝╚═════╝░
;;

;; Removing wayland input lags !!IMPORTANT
(setq-default pgtk-wait-for-event-timeout 0)

;; No mode line
(setq-default mode-line-format nil)

;; No startup  screen
(setq inhibit-startup-screen t)

;; startup message
(setq server-client-instructions nil)
(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area-message t)

;; No message in scratch buffer
(setq initial-scratch-message nil)

;; Initial buffer
(setq initial-buffer-choice nil)

;; No frame title
(setq frame-title-format nil)

;; No file dialog
(setq use-file-dialog nil)

;; No dialog box
(setq use-dialog-box nil)

;; No popup windows
(setq pop-up-windows nil)

;; No empty line indicators
(setq indicate-empty-lines nil)

;; No cursor in inactive windows
(setq-default cursor-in-non-selected-windows nil)

;; No scroll bars
(if (fboundp 'scroll-bar-mode) (set-scroll-bar-mode nil))

;; No toolbar
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))

;; No menu bar
(menu-bar-mode -1)

;; No tabs
(setq-default indent-tabs-mode nil)

;; Tab.space equivalence
(setq-default tab-width 4)

;; Size of temporary buffers
(temp-buffer-resize-mode)
(setq-default temp-buffer-max-height 8)

;; Minimum window height
(setq-default window-min-height 1)

;; Tab behavior
;; (setq tab-always-indent 'complete)
;; (global-company-mode)
;; (define-key company-mode-map [remap indent-for-tab-command]
;;   #'company-indent-or-complete-common)


;; scroll if reached the end only
(setq-default scroll-margin 0 )

;;When you scroll down, and up again, point should end up at the same position you started out with
(setq scroll-preserve-screen-position t)

;; Scroll line wise
(setq-default scroll-step 1)

;; What do these do?
(setq-default scroll-conservatively  10000)

;; Moderate font lock
;; Disabling syntax highlights
;; (setq font-lock-maximum-decoration nil)

;; No limit on font lock (tree style levels)
(setq-default font-lock-maximum-size nil)

;; No line break space points
(setq-default auto-fill-mode nil)

;; Better autofill --  is nil by default
;; (setq refill-mode nil)

;; No confirmation for visiting non-existent files
(setq-default confirm-nonexistent-file-or-buffer nil)

;; Use RET to open org-mode links, including those in quick-help.org
(setq-default org-return-follows-link t)

;; Internal border / padding
;; (set-frame-parameter (selected-frame) 'internal-border-width 20)
(add-to-list 'default-frame-alist '(internal-border-width . 20 ))
(add-to-list 'default-frame-alist '(internal-show-cursor . -1))

;; Disable the line break symbols
(setf (cdr (assq 'continuation fringe-indicator-alist)) '(nil nil))

;; Disable the $ symbol too
(set-display-table-slot standard-display-table 'truncation ?\)

;; Disable fringe
(fringe-mode '(0 . 0))

;; Display Minimum warnings
(setq-default warning-minimum-level :emergency)

;; Display line numbers globally
(global-display-line-numbers-mode t)

;; Disable line number mode in a few situations
(dolist (mode '(term-mode-hook treemacs-mode-hook shell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; Revert buffers when the underlying file has changed
(global-auto-revert-mode 1)

;; Revert Dired and other buffers
(setq-default global-auto-revert-non-file-buffers t)

;; Enable recentf
(require 'recentf)
(recentf-mode 1)

;; Silence compiler warnings as they can be pretty disruptive
(if (boundp 'comp-deferred-compilation)
    (setq-default comp-deferred-compilation nil)
  (setq-default native-comp-deferred-compilation nil))

(setq-default native-comp-async-report-warnings-errors nil)

;; In noninteractive sessions, prioritize non-byte-compiled source files to
;; prevent the use of stale byte-code. Otherwise, it saves us a little IO time
;; to skip the mtime checks on every *.elc file.
(setq-default load-prefer-newer noninteractive)

;; No ugly button for checkboxes
(setq-default widget-image-enable nil)

;; Disable Blink cursor
(blink-cursor-mode -1)

;; Disable signatures and stuff on minibuf
(global-eldoc-mode 1)

;; Save my last place
(save-place-mode 1)

;; Move customization variables to a separate file and load it
(setq-default custom-file (locate-user-emacs-file "custom-vars.el"))
(load custom-file 'noerror 'nomessage)

;; Visual line mode : llllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllll
(set-default 'truncate-lines t)

;; Dired fixes
(setq dired-use-ls-dired nil)

;; Raise undo-limit to 80Mb
(setq-default undo-limit 80000000)

;; Autosave true
(setq-default auto-save-default t)


;; ease of life
(fset 'yes-or-no-p 'y-or-n-p)

;; itterate through CamelCase words
;; (global-subword-mode 1)

(setq-default delete-by-moving-to-trash t); Delete files to trash

(setq-default delete-selection-mode t)

;; Disabling message logs
;; (setq-default message-log-max nil)

;; (setq max-mini-window-height 1) ; Don't let echo area grow;;

;; disable the delays
(setq-default show-paren-delay 0)
(setq-default eldoc-idle-delay 0)

;; Disable default css colors
(setq-default css-fontify-colors nil)

;; empty line at the end
(setq require-final-newline t)

;; NECESSARY
(provide 'options)
