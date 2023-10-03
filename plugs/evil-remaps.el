;;
;; ███████╗██╗░░░██╗██╗██╗░░░░░
;; ██╔════╝██║░░░██║██║██║░░░░░
;; █████╗░░╚██╗░██╔╝██║██║░░░░░
;; ██╔══╝░░░╚████╔╝░██║██║░░░░░
;; ███████╗░░╚██╔╝░░██║███████╗
;; ╚══════╝░░░╚═╝░░░╚═╝╚══════╝
;;  󰱯 
;;  󰇴 

;; ENABLE

(evil-mode 1)
(evil-collection-init)
(global-evil-surround-mode 1)
(evil-commentary-mode 1)
(undo-fu-session-global-mode)

;; FUNCTIONS
 (defun clear ()
   (interactive)
   (evil-ex-nohighlight)
   (posframe-hide-all))

 (defun configure-evil-esc ()
   "Default escape key"
   (evil-escape-mode 1))
 (add-hook 'evil-insert-state-entry-hook #'configure-evil-esc)

;; To prevent the visual mode lag:
 (defun configure-evil-exit-esc ()
   "Default escape key"
   (evil-escape-mode -1))
 (add-hook 'evil-visual-state-entry-hook #'configure-evil-exit-esc)


;; OPTIONS
(setq evil-want-keybinding nil)
(setq evil-want-integration t) ;; This is optional since it's already set to t by default.
(setq evil-want-C-i-jump nil)
(setq evil-want-C-u-scroll t)
(setq evil-want-C-d-scroll t)
(setq evil-want-C-h-delete nil)
(setq evil-want-fine-undo t) ; By default while in insert all changes are one big blob. Be more granular
(setq evil-want-Y-yank-to-eol t)
(setq evil-collection-elpaca-want-v t)
(evil-select-search-module 'evil-search-module 'evil-search)

;; UNBIND SOME KEYS
(general-unbind 'normal
  "C-j"
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
  "C-u" #'evil-scroll-up
  "C-d" #'evil-scroll-down
  "C-l" 'clear
  "M-k" 'drag-stuff-up
  "M-j" 'drag-stuff-down
  "M-h" 'drag-stuff-left
  "M-l" 'drag-stuff-right
  "C-/" #'consult-line-multi
  "<tab>" 'next-buffer ;;easier nav
  "<backtab>" 'previous-buffer ;;easier nav
  )
;; Insert State
(general-def
  :keymaps 'evil-insert-state-map
  "C-h" 'nil
  "C-k" 'corfu-previous
  "C-j" 'corfu-next
  "C-l" 'completion-at-point
  "ESC" 'corfu-quit
  )

(provide 'evil-remaps)
