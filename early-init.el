;; Centralize Backup
;; For DEMO.el~
(setq backup-directory-alist `(("." . ,(expand-file-name "tmp/backups/" user-emacs-directory))))

;; For #DEMO.el#
;; auto-save-mode doesn't create the path automatically!
(make-directory (expand-file-name "tmp/auto-saves/" user-emacs-directory) t)

(setq auto-save-list-file-prefix (expand-file-name "tmp/auto-saves/sessions/" user-emacs-directory)
      auto-save-file-name-transforms `((".*" ,(expand-file-name "tmp/auto-saves/" user-emacs-directory) t)))

;; For #DEMO.el
;; This is used to protect file from being edited by another emacs session while its unsaved
;; (setq create-lockfiles nil)

(setq projectile-known-projects-file (expand-file-name "tmp/projectile-bookmarks.eld" user-emacs-directory)
      lsp-session-file (expand-file-name "tmp/.lsp-session-v1" user-emacs-directory))

(setq user-emacs-directory (expand-file-name "~/.cache/emacs"))

(setq-default recentf-save-file "~/.cache/emacs/recentf")

;; Disable "Beginning of Buffer" or "End of Buffer"
(defun my-command-error-function (data context caller)
  "Ignore the buffer-read-only, beginning-of-buffer,
end-of-buffer signals; pass the rest to the default handler."
  (when (not (memq (car data) '(buffer-read-only
                                beginning-of-buffer
                                end-of-buffer)))
    (command-error-default-function data context caller)))

(setq command-error-function #'my-command-error-function)

;; (defun suppress-message-advice-around (fun &rest args)
;;   (let (message-log-max)
;;     (with-temp-message (or (current-message) "")
;;       (apply fun args))))

;; ;; example: suppress any messages from `save-buffer'
;; (advice-add 'save-buffer :around 'suppress-message-advice-around)
;; (advice-add 'write-file :around 'suppress-message-advice-around)
;; (advice-add 'evil-save :around 'suppress-message-advice-around)
;; (advice-add 'kill-buffer :around 'suppress-message-advice-around)

;; ;; Need to do this before setting the evil keybindings
(setq evil-want-keybinding nil)

;; -*- lexical-binding: t; -*-

;; -------------------------------------------------------------------------------- ;;
;; This early-init.el file was auto-tangled from an orgmode file. (C) Jake B        ;;
;; -------------------------------------------------------------------------------- ;;

;; Garbage Collections
(setq gc-cons-percentage 0.6)

;; Compile warnings
;;  (setq warning-minimum-level :emergency)
(setq native-comp-async-report-warnings-errors 'silent) ;; native-comp warning
(setq byte-compile-warnings '(not free-vars unresolved noruntime lexical make-local))


;; MISC OPTIMIZATIONS ----
;;; optimizations (froom Doom's core.el). See that file for descriptions.
(setq idle-update-delay 1.0)

;; Disabling bidi (bidirectional editing stuff)
(setq-default bidi-display-reordering 'left-to-right
              bidi-paragraph-direction 'left-to-right)

(setq-default cursor-in-non-selected-windows nil)
(setq highlight-nonselected-windows nil)
(setq fast-but-imprecise-scrolling t)
