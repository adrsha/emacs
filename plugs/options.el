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
(setq cursor-in-non-selected-windows nil)

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
(setq temp-buffer-max-height 8)

;; Minimum window height
(setq window-min-height 1)

;; Tab behavior
;; (setq tab-always-indent 'complete)
;; (global-company-mode)
;; (define-key company-mode-map [remap indent-for-tab-command]
;;   #'company-indent-or-complete-common)


;; scroll if reached the end only
(setq scroll-margin 0 )

;;When you scroll down, and up again, point should end up at the same position you started out with
(setq scroll-preserve-screen-position t)

;; Scroll line wise
(setq scroll-step 1)

;; What do these do?
(setq scroll-conservatively  10000)


;; Moderate font lock
;; Disabling syntax highlights
;; (setq font-lock-maximum-decoration nil)

;; No limit on font lock (tree style levels)
(setq font-lock-maximum-size nil)

;; No line break space points
(setq auto-fill-mode nil)

;; Better autofill --  is nil by default
;; (setq refill-mode nil)

;; Fill column at 80
(setq fill-column 80)

;; No confirmation for visiting non-existent files
(setq confirm-nonexistent-file-or-buffer nil)

;; Completion style, see
;; gnu.org/software/emacs/manual/html_node/emacs/Completion-Styles.html
(setq completion-styles '(basic substring partial-completion ))

;; Use RET to open org-mode links, including those in quick-help.org
(setq org-return-follows-link t)

;; Internal border / padding
;; (set-frame-parameter (selected-frame) 'internal-border-width 20)
(add-to-list 'default-frame-alist '(internal-border-width . 20 ))

;; Fringe Disable
(setq set-fringe-mode 0)
;; Display Minimum warnings
(setq warning-minimum-level :emergency)

;; Display line numbers globally
(global-display-line-numbers-mode t)

;; Disable line number mode in a few situations
(dolist (mode '(term-mode-hook shell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; Revert buffers when the underlying file has changed
(global-auto-revert-mode 1)

;; Revert Dired and other buffers
(setq global-auto-revert-non-file-buffers t)

;; Enable recentf
(require 'recentf)
(recentf-mode 1)

;; Silence compiler warnings as they can be pretty disruptive
(if (boundp 'comp-deferred-compilation)
    (setq comp-deferred-compilation nil)
  (setq native-comp-deferred-compilation nil))

(setq native-comp-async-report-warnings-errors nil)

;; In noninteractive sessions, prioritize non-byte-compiled source files to
;; prevent the use of stale byte-code. Otherwise, it saves us a little IO time
;; to skip the mtime checks on every *.elc file.
(setq load-prefer-newer noninteractive)

;; No ugly button for checkboxes
(setq widget-image-enable nil)

;; Disable Blink cursor
(blink-cursor-mode -1)

;; Disable signatures and stuff on minibuf
(global-eldoc-mode 1)

;; Save my last place
(save-place-mode 1)

;; Move customization variables to a separate file and load it
(setq custom-file (locate-user-emacs-file "custom-vars.el"))
(load custom-file 'noerror 'nomessage)

; Raise undo-limit to 80Mb
(setq undo-limit 80000000)

;; Autosave true
(setq auto-save-default t)

;; ease of life
(fset 'yes-or-no-p 'y-or-n-p)

;; itterate through CamelCase words
;; (global-subword-mode 1)

(setq-default delete-by-moving-to-trash t); Delete files to trash

(setq-default delete-selection-mode t)

(setq-default message-log-max 5)
;; (setq max-mini-window-height 1) ; Don't let echo area grow;; (setq max-mini-window-height 1) ; Don't let echo area grow;; (setq max-mini-window-height 1) ; Don't let echo area grow;; (setq max-mini-window-height 1) ; Don't let echo area grow

(visual-line-mode 1)

;; disable the delays
(setq show-paren-delay 0)
(setq eldoc-idle-delay 0)

;; Disable default colors
(setq css-fontify-colors nil)
;; NECESSARY
(provide 'options)
