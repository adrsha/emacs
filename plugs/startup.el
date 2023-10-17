
;; ░██████╗████████╗░█████╗░██████╗░████████╗██╗░░░██╗██████╗░
;; ██╔════╝╚══██╔══╝██╔══██╗██╔══██╗╚══██╔══╝██║░░░██║██╔══██╗
;; ╚█████╗░░░░██║░░░███████║██████╔╝░░░██║░░░██║░░░██║██████╔╝
;; ░╚═══██╗░░░██║░░░██╔══██║██╔══██╗░░░██║░░░██║░░░██║██╔═══╝░
;; ██████╔╝░░░██║░░░██║░░██║██║░░██║░░░██║░░░╚██████╔╝██║░░░░░
;; ╚═════╝░░░░╚═╝░░░╚═╝░░╚═╝╚═╝░░╚═╝░░░╚═╝░░░░╚═════╝░╚═╝░░░░░
;;
;;


;; Disable default package manager
(setq package-enable-at-startup nil)
(setq warning-suppress-log-types '((package reinitialization)))

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

(defun suppress-message-advice-around (fun &rest args)
  (let (message-log-max)
    (with-temp-message (or (current-message) "")
      (apply fun args))))

;; example: suppress any messages from `save-buffer'
(advice-add 'save-buffer :around 'suppress-message-advice-around)
(advice-add 'kill-buffer :around 'suppress-message-advice-around)

;; Need to do this before setting the evil keybindings
(setq evil-want-keybinding nil)
(provide 'startup)
