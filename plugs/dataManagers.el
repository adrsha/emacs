
;; ██████╗░░█████╗░████████╗░█████╗░  ███╗░░░███╗░█████╗░███╗░░██╗
;; ██╔══██╗██╔══██╗╚══██╔══╝██╔══██╗  ████╗░████║██╔══██╗████╗░██║
;; ██║░░██║███████║░░░██║░░░███████║  ██╔████╔██║███████║██╔██╗██║
;; ██║░░██║██╔══██║░░░██║░░░██╔══██║  ██║╚██╔╝██║██╔══██║██║╚████║
;; ██████╔╝██║░░██║░░░██║░░░██║░░██║  ██║░╚═╝░██║██║░░██║██║░╚███║
;; ╚═════╝░╚═╝░░╚═╝░░░╚═╝░░░╚═╝░░╚═╝  ╚═╝░░░░░╚═╝╚═╝░░╚═╝╚═╝░░╚══╝
;;
;;

;; Setup Projectile
(projectile-mode)

;; Breadcrumb
(require 'breadcrumb)
(when (file-directory-p "/hdd/Documents")
  (setq projectile-project-search-path '("/hdd/Documents")))
(setq projectile-switch-project-action #'projectile-dired)

;; Breadcrumb
(require 'breadcrumb)
(breadcrumb-mode)

;;-------------------- TREEMACS -------------------;;

;; Setup treesmacs
(progn
  (setq treemacs-collapse-dirs                   (if treemacs-python-executable 3 0)
        treemacs-deferred-git-apply-delay        0.5
        treemacs-directory-name-transformer      #'identity
        treemacs-display-in-side-window          t
        treemacs-eldoc-display                   'simple
        treemacs-file-event-delay                2000
        treemacs-file-extension-regex            treemacs-last-period-regex-value
        treemacs-file-follow-delay               0
        treemacs-file-name-transformer           #'identity
        treemacs-follow-after-init               t
        treemacs-expand-after-init               t
        treemacs-find-workspace-method           'find-for-file-or-pick-first
        treemacs-git-command-pipe                ""
        treemacs-goto-tag-strategy               'refetch-index
        treemacs-header-scroll-indicators        '(nil . "┴┴┴┴┴┴")
        treemacs-hide-dot-git-directory          t
        treemacs-indentation                     2
        treemacs-indentation-string              " "
        treemacs-is-never-other-window           nil
        treemacs-max-git-entries                 5000
        treemacs-missing-project-action          'ask
        treemacs-move-forward-on-expand          nil
        treemacs-no-png-images                   nil
        treemacs-no-delete-other-windows         t
        treemacs-project-follow-cleanup          t
        treemacs-persist-file                    nil
        treemacs-position                        'left
        treemacs-read-string-input               'from-childframe
        treemacs-recenter-distance               0.1
        treemacs-recenter-after-file-follow      'nil
        treemacs-recenter-after-tag-follow       'nil
        treemacs-recenter-after-project-jump     'nil
        treemacs-recenter-after-project-expand   'nil
        treemacs-litter-directories              '("/node_modules" "/.venv" "/.cask")
        treemacs-project-follow-into-home        'nil
        treemacs-show-cursor                     nil
        treemacs-show-hidden-files               t
        treemacs-silent-filewatch                t
        treemacs-silent-refresh                  t
        treemacs-sorting                         'alphabetic-asc
        treemacs-select-when-already-in-treemacs 'move-back
        treemacs-space-between-root-nodes        t
        treemacs-tag-follow-cleanup              t
        treemacs-tag-follow-delay                1.5
        treemacs-text-scale                      nil
        treemacs-user-mode-line-format           'none
        treemacs-user-header-line-format         nil
        treemacs-wide-toggle-width               70
        treemacs-width                           35
        treemacs-width-increment                 1
        treemacs-width-is-initially-locked       t
        treemacs-workspace-switch-cleanup        nil)

  ;; The default width and height of the icons is 22 pixels. If you are
  ;; using a Hi-DPI display, uncomment this to double the icon size.
  ;;(treemacs-resize-icons 44)

  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t)
  (treemacs-fringe-indicator-mode 'always)

  (when treemacs-python-executable
    (treemacs-git-commit-diff-mode t))

  (pcase (cons (not (null (executable-find "git")))
               (not (null treemacs-python-executable)))
    (`(t . t)
     (treemacs-git-mode 'deferred))
    (`(t . _)
     (treemacs-git-mode 'simple)))

  (treemacs-hide-gitignored-files-mode nil))

;; Modifying icons
(treemacs-modify-theme "Default"
  :icon-directory "~/.config/emacs/images/"
  :config
  (progn
    (treemacs-create-icon :file "folder-open.png"   :extensions (root-open))
    (treemacs-create-icon :file "folder-asterick.png"   :extensions (root-closed))
    (treemacs-create-icon :file "org.png"   :extensions ("org"))
    (treemacs-create-icon :file "file.png"   :extensions (fallback))
    (treemacs-create-icon :file "emacs.png" :extensions ("el"))
    (treemacs-create-icon :file "folder-open.png" :extensions (dir-open))
    (treemacs-create-icon :file "folder.png" :extensions (dir-closed))))


(dolist (mode '(treemacs-mode-hook ))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(setq treemacs-width 25)
;;-------------------- TREEMACS -------------------;;


;; Popper
(popper-mode 1)
;; Match eshell, shell, term and/or vterm buffers
(setq popper-reference-buffers
      (append popper-reference-buffers
              '("^\\*eshell.*\\*$" eshell-mode ;eshell as a popup
                "^\\*shell.*\\*$"  shell-mode  ;shell as a popup
                "^\\*term.*\\*$"   term-mode   ;term as a popup
                "^\\*vterm.*\\*$"  vterm-mode  ;vterm as a popup
                )))


;; DIRed
(setq dired-listing-switches "-Al --group-directories-first")
(setq-default dired-kill-when-opening-new-dired-buffer 't)

(treemacs-icons-dired-mode 1)
(defun use-betterfonts-dired ()
  "Switch the current buffer to a monospace font."
  (face-remap-add-relative 'default '(:family "Barlow Semi Condensed")))

(add-hook 'dired-mode-hook 'use-betterfonts-dired)
(add-hook 'dired-mode-hook 'dired-hide-details-mode)





(provide 'dataManagers)
