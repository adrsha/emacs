(setq evil-want-keybinding nil)
(setq evil-want-integration t) ;; This is optional since it's already set to t by default.
(setq evil-want-C-i-jump nil)
(setq evil-want-fine-undo t) ; By default while in insert all changes are one big blob. Be more granular
(setq evil-want-Y-yank-to-eol t)
(evil-select-search-module 'evil-search-module 'evil-search)
(evil-mode 1)

(evil-collection-init)
(define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
(evil-global-set-key 'motion "j" 'evil-next-visual-line)
(evil-global-set-key 'motion "k" 'evil-previous-visual-line)
(evil-set-initial-state 'messages-buffer-mode 'normal)
(evil-set-initial-state 'dashboard-mode 'normal)


 (defun configure-evil-esc ()
   "Default escape key"
   (evil-escape-mode 1))
 (add-hook 'evil-insert-state-entry-hook #'configure-evil-esc)

;; To prevent the visual mode lag:
 (defun configure-evil-exit-esc ()
   "Default escape key"
   (evil-escape-mode -1))
 (add-hook 'evil-visual-state-entry-hook #'configure-evil-exit-esc)

 (setq-default evil-escape-key-sequence "jk" evil-escape-delay 0.3)

(global-evil-surround-mode 1)
 (evil-commentary-mode 1)

(general-def
  :keymaps 'evil-normal-state-map
  "C-r" #'undo-redo
  "u" #'undo-only
  "C-u" #'evil-scroll-up
  "C-d" #'evil-scroll-down
  "C-l" #'evil-ex-nohighlight
  "M-k" 'drag-stuff-up
  "M-j" 'drag-stuff-down
  "M-h" 'drag-stuff-left
  "M-l" 'drag-stuff-right
  "C-/" #'consult-line-multi
  "<tab>" 'next-buffer ;;easier nav
  "<backtab>" 'previous-buffer ;;easier nav
  )


(general-def
  :keymaps 'evil-insert-state-map
  "C-k" 'corfu-previous
  "C-j" 'corfu-next
  "C-l" 'corfu-complete
  )

(general-unbind 'normal
  "C-j"
  "C-l"
  "K"
  "C-k")


(provide 'evil-remaps)
