;;-*- lexical-binding: t; -*-

;; INFO:  Mode specific maps
;; (general-def org-mode-map
;;   "C-c C-q" 'counsel-org-tag
;;   ;; ...
;;   )

;; INFO: normal maps
;; (general-define-key
;;  "M-x" 'amx
;;  "C-s" 'counsel-grep-or-swiper)

;; INFO: prefix
;; (general-define-key
;;  :prefix "C-c"
;;  ;; bind "C-c a" to 'org-agenda
;;  "a" 'org-agenda
;;  "b" 'counsel-bookmark
;;  "c" 'org-capture)

;; INFO: Swap!
;; (general-swap-key nil 'motion
;;   ";" ":")

;;custom functions

(general-unbind 'normal
  "C-j"
  "C-l"
  "C-k")

(general-def
  "C-j" 'nil
  "C-k" 'nil)

;; (define-key vertico-map (kbd "TAB") 'vertico-next)
;; (keymap-set vertico-map "C-l" '(lambda () (interactive) (vertico-insert) (minibuffer-force-complete-and-exit)))

;; Using RETURN to follow links in Org/Evil
;; Unmap keys in 'evil-maps if not done, (setq org-return-follows-link t) will not work

;; GNRL
(provide 'keymaps)
;; Globals
;; (global-set-key (kbd "M-b") 'popper-toggle)
;; (global-set-key (kbd "M-n") 'popper-cycle)
;; (global-set-key (kbd "M-B") 'popper-toggle-type)
;; (global-set-key (kbd "M-q") 'popper-kill-latest-popup)

;;Embark
;; (define-key embark-file-map     (kbd "o") (my/embark-ace-action find-file))
;; (define-key embark-buffer-map   (kbd "o") (my/embark-ace-action switch-to-buffer))
;; (define-key embark-bookmark-map (kbd "o") (my/embark-ace-action bookmark-jump))

;; LEADER KEY

(general-create-definer e/leader-keys
  :keymaps '(normal insert visual emacs)
  :prefix "SPC"
  :global-prefix "C-SPC")

(e/leader-keys
  "SPC" '(execute-extended-command :which-key "  M-x")
  "k"  '(eldoc-box-help-at-point :which-key "󰆽 hover")
  ;;LSP
  "c"  '(:ignore t :which-key "󰅱 code")
  "cn"  '(flycheck-next-error :which-key " next error")
  "cp"  '(flycheck-previous-error :which-key " next error")
  "cr"  '(eglot-rename :which-key " rename symbol")
  "cc"  '(format-all-buffer :which-key " format region or buffer")
  "cl"  '(crux-cleanup-buffer-or-region :which-key " cleanup")
  "cd"  '(crux-duplicate-current-line-or-region :which-key "󰬸  duplicate")

  "f"  '(:ignore t :which-key "󰈔 files")
  "ff" '(find-file :which-key "󰈞 find a file")
  "fd" '(find-dired :which-key "󰈞 find a file")
  "fr" '(consult-recent-file :which-key "󰣜 recent files")
  "fc"  '(:ignore t :which-key "󰈔 current file")
  "fcs" '(crux-sudo-edit :which-key "󱄛 open this file as sudo")
  "fcr" '(crux-rename-buffer-and-file :which-key "󰑕 rename this file")

  "o"  '(:ignore t :which-key "󰷏 open")
  "ow" '(crux-open-with :which-key " open with")
  "or" '(consult-yank-from-kill-ring :which-key " open registry and yank")
  "oF" '(list-faces-display :which-key " open with")

  "b"  '(:ignore t :which-key " buffer navigation")
  "bd" '(kill-buffer-and-window :which-key "󰆴 kill the current buffer and window")
  "bk" '(crux-kill-other-buffers :which-key "󰛌 kill all other buffers and windows")
  "bn" '(next-buffer :which-key "󰛂 switch buffer")
  "bp" '(previous-buffer :which-key "󰛁 switch buffer")
  "bb" '(consult-buffer :which-key "󰕰 view buffers")

  "s"  '(:ignore t :which-key " search")
  "ss" '(consult-line :which-key "󰱼 line search")
  "si" '(nerd-icons-insert :which-key "󰭟 search for icons")
  "srg" '(consult-ripgrep :which-key "󰟥 search with rg")
  "sd" '(dictionary-search :which-key "󰬋 search in dictionary")

  "e"  '(:ignore t :which-key "󰈈  evaluate")
  "eb" '(eval-buffer :which-key "󰷊  evaluate buffer")
  "ear" '(crux-eval-and-replace :which-key "󰛔 evaluate and replace")
  "er" '(eval-region :which-key "󰨺  evaluate region")

  "p"  '(:ignore t :which-key "󰅱 project")
  "pr"  '(projectile-recentf :which-key "󰈞 recentf")
  "pv"  '(:ignore t :which-key "󰅱 view")
  "pvc"  '(projectile-vc :which-key "󰈞 view changes")
  "pvd"  '(projectile-browse-dirty-projects :which-key "󰈞 view dirty projects")
  "ps"  '(:ignore t :which-key "󰅱 switch")
  "psp"  '(projectile-switch-open-project :which-key "󰅱 switch project")

  "h"  '(:ignore t :which-key "󰞋 help")
  "ht" '(helpful-at-point :which-key " describe this")
  "hF" '(describe-face :which-key "󱗎 describe face")
  "hf" '(helpful-function :which-key "󰯻 describe function")
  "hb" '(embark-bindings :which-key "󰌌 describe bindings")
  "hk" '(helpful-key :which-key "󰯻 describe this key")
  "hv" '(helpful-variable :which-key " describe variable")
  "hr" '(:ignore t :which-key "󱍸 reload")
  "hrb" '(revert-buffer-quick :which-key "󰄸 reload buffer")
  "hrr" '((lambda () (interactive) (load-file "~/.config/emacs/init.el")) :wk "  Reload emacs config")

  "t"  '(:ignore t :which-key "  toggles")
  "tv"  '(visual-line-mode :which-key "󰖶 toggle visual line mode")
  "tm"  '(minimap-mode :which-key "󰍍 minimap toggles")
  )

(general-def
  "C-<return>" 'embark-act
  "K" 'nil
  "<escape>" 'keyboard-escape-quit)

;; (general-def
;;   :keymaps 'acm-mode-map
;;   "C-l" 'acm-complete
;;   "C-j" 'acm-select-next
;;   "C-k" 'acm-select-prev)

(general-def
  :keymaps 'vertico-map
  "TAB" #'vertico-insert
  "C-l" '(lambda () (interactive) (vertico-insert) )
  "C-j" #'vertico-next
  "C-k" #'vertico-previous
  "C-h" #'vertico-directory-up
  "M-o" #'vertico-repeat
  )

;;DRAG
(general-def
  :keymaps 'evil-normal-state-map
  "M-k" 'drag-stuff-up
  "M-j" 'drag-stuff-down
  "M-h" 'drag-stuff-left
  "M-l" 'drag-stuff-right
  "<tab>" 'next-buffer ;;easier nav
  "<backtab>" 'previous-buffer ;;easier nav
  "C-/" 'consult-line-multi)

(general-def
  :keymaps 'evil-insert-state-map
  "C-k" 'corfu-previous
  "C-j" 'corfu-next
  "C-l" 'corfu-complete
  )

;;LSP

;; for embark
;;; keymaps.el ends here
