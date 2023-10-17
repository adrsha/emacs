;;
;; ███████╗██╗░░░██╗██╗██╗░░░░░
;; ██╔════╝██║░░░██║██║██║░░░░░
;; █████╗░░╚██╗░██╔╝██║██║░░░░░
;; ██╔══╝░░░╚████╔╝░██║██║░░░░░
;; ███████╗░░╚██╔╝░░██║███████╗
;; ╚══════╝░░░╚═╝░░░╚═╝╚══════╝
;;  󰱯 
;;  󰇴 

;; OPTIONS
(setq evil-want-integration t) ;; This is optional since it's already set to t by default.
(setq evil-want-C-i-jump nil)
(setq evil-want-C-u-scroll t)
(setq evil-want-C-d-scroll t)
(setq evil-want-C-h-delete nil)
(setq evil-want-fine-undo t) ; By default while in insert all changes are one big blob. Be more granular
(setq evil-want-Y-yank-to-eol t)
(setq evil-collection-elpaca-want-v t)
(evil-select-search-module 'evil-search-module 'evil-search)

;; ENABLE

(evil-mode 1)
(evil-collection-init)
(global-evil-surround-mode 1)
(global-evil-mc-mode 1)
(evil-commentary-mode 1)
(undo-fu-session-global-mode)

;; FUNCTIONS
 (defun clear ()
   (interactive)
   (evil-ex-nohighlight)
   (posframe-hide-all)
   (evil-mc-undo-all-cursors)
   (evil-force-normal-state)
   (pulsar-pulse-line))

 (defun configure-evil-ins ()
   "Default evil ins key"
   (evil-escape-mode 1))
 (add-hook 'evil-insert-state-entry-hook #'configure-evil-ins)
 (add-hook 'minibuffer-mode-hook #'(lambda () (interactive) (evil-escape-mode 1) ))


;; To prevent the visual mode lag:
 (defun configure-evil-exit-ins ()
   "Default evil ins key"
   (evil-escape-mode -1))
 (add-hook 'evil-visual-state-entry-hook #'configure-evil-exit-ins)


;; UNBIND SOME KEYS
(general-unbind 'normal
  "C-j"
  "M-x"
  "K"
  "C-k")

;; INITIAL BINDINGS
(evil-global-set-key 'motion "j" 'evil-next-visual-line)
(evil-global-set-key 'motion "k" 'evil-previous-visual-line)
(evil-set-initial-state 'messages-buffer-mode 'normal)
(evil-set-initial-state 'dashboard-mode 'normal)

;; Escape key
(setq-default evil-escape-key-sequence "jk" evil-escape-delay 0.3)

;; Normal state
(general-def
  :keymaps 'evil-normal-state-map
  "C-r" #'undo-redo
  "u" #'undo-only
  "\\" #'treemacs-select-window
  "C-u" #'evil-scroll-up
  "C-d" #'evil-scroll-down
  "C-h" 'er/expand-region
  "C-S-h" 'er/contract-region
  "C-s" (lambda () (interactive) (evil-ex "%s/"))
  "C-l" 'clear
  "RET" 'org-open-at-point-global
  "M-k" 'drag-stuff-up
  "M-j" 'drag-stuff-down
  "M-h" 'drag-stuff-left
  "M-l" 'drag-stuff-right
  "C-/" #'consult-line-multi
  "C-j" #'evil-mc-make-and-goto-next-match
  "C-S-j" #'evil-mc-skip-and-goto-next-match
  "C-k" #'evil-mc-make-and-goto-prev-match
  "C-S-k" #'evil-mc-skip-and-goto-prev-match
  "C-S-p" #'evil-mc-undo-last-added-cursor
  "C-a" #'evil-mc-make-all-cursors
  )
;; Insert State

(general-def
  :keymaps 'evil-insert-state-map
  "C-h" 'nil
  "C-k" 'corfu-previous
  "C-j" 'corfu-next
  "C-." 'yas-expand
  "C-l" 'completion-at-point
  "C-e" 'corfu-quit
  "C-f" 'find-file-at-point
  )

;; Org-mode
(general-def
  :keymaps 'org-mode-map
  "C-h" 'org-toggle-heading
  )

(evil-collection-define-key 'normal 'dired-mode-map
  "l" 'dired-find-alternate-file
  "h" 'dired-up-directory
  "c" 'dired-create-empty-file
  "Q" 'kill-buffer-and-window
  )

;;  (evil-define-key 'motion 'dired-mode-map "Q" 'kill-this-buffer)
(evil-define-key 'motion help-mode-map "q" 'kill-this-buffer)
(evil-define-key 'motion calendar-mode-map "q" 'kill-this-buffer)

(provide 'evil-remaps)
