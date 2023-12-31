(org-babel-load-file (locate-user-emacs-file "init.org"))

;; EMACS UIs

(setq-default mode-line-format nil)

(setq-default cursor-in-non-selected-windows nil)

;;(setq inhibit-startup-screen t)

(setq server-client-instructions nil)
;;(setq inhibit-startup-echo-area-message t)

(setq initial-scratch-message nil)

(setq initial-buffer-choice nil)

(setq frame-title-format nil)

(if (fboundp 'scroll-bar-mode) (set-scroll-bar-mode nil))

(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))

(menu-bar-mode -1)

(blink-cursor-mode -1)

(global-display-line-numbers-mode t)

(set-default 'truncate-lines t)

(setq-default inhibit-message nil)

(add-to-list 'default-frame-alist '(internal-border-width . 10 ))
(add-to-list 'default-frame-alist '(internal-show-cursor . -1))

(set-display-table-slot standard-display-table 0 ?\ )

(setf (cdr (assq 'continuation fringe-indicator-alist)) '(nil nil))

(setq-default widget-image-enable nil)

(defun my-command-error-function (data context caller)
  "Ignore the buffer-read-only, beginning-of-buffer,
 end-of-buffer signals; pass the rest to the default handler."
  (when (not (memq (car data) '(buffer-read-only
                                beginning-of-buffer
                                end-of-buffer)))
    (command-error-default-function data context caller)))

(setq command-error-function #'my-command-error-function)

(defun suppress-message-advice-around (fun &rest args)
  (let (message-log-max)
    (with-temp-message (or (current-message) "")
      (apply fun args))))

;; example: suppress any messages from `save-buffer'
(advice-add 'save-buffer :around 'suppress-message-advice-around)
(advice-add 'kill-buffer :around 'suppress-message-advice-around)

;; Disable echoing keystrokes
(setq-default echo-keystrokes 0.025)

;; Disable Evil Modes information
(setq-default evil-echo-state nil)

;; File syntax: DEMO.el~
(setq backup-directory-alist `(("." . ,(expand-file-name "tmp/backups/" user-emacs-directory))))

;; File syntax #DEMO.el#

;; auto-save-mode doesn't create the path automatically!
(make-directory (expand-file-name "tmp/auto-saves/" user-emacs-directory) t)

(setq auto-save-list-file-prefix (expand-file-name "tmp/auto-saves/sessions/" user-emacs-directory) ; prefix for generating autosave list file name
      auto-save-file-name-transforms `((".*" ,(expand-file-name "tmp/auto-saves/" user-emacs-directory) t)))

;; For #DEMO.el
;; (setq create-lockfiles nil)

(setq user-emacs-directory (expand-file-name "~/.cache/emacs"))

(setq-default recentf-save-file "~/.config/emacs/recentf")
