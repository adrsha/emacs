;;-*- lexical-binding: t; -*-
;; Make ESC quit prompts

(use-package crux)
(use-package drag-stuff)

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; (define-key vertico-map (kbd "TAB") 'vertico-next)
;; (keymap-set vertico-map "C-l" '(lambda () (interactive) (vertico-insert) (minibuffer-force-complete-and-exit)))
(keymap-set vertico-map "C-l" '(lambda () (interactive) (vertico-insert) ))
(keymap-set vertico-map "TAB" #'vertico-insert)
(keymap-set vertico-map "C-j" #'vertico-next)
(keymap-set vertico-map "C-k" #'vertico-previous)
(keymap-set vertico-map "C-h" #'vertico-directory-up)
(keymap-set vertico-map "M-o" #'vertico-repeat)

(keymap-set global-map "C-k" #'nil)
(keymap-set global-map "C-j" #'nil)
(keymap-set evil-insert-state-map "C-j" #'nil)
(keymap-set evil-insert-state-map "C-k" #'nil)

(keymap-set acm-mode-map "C-l" 'acm-complete)
(keymap-set acm-mode-map "C-j" 'acm-select-next)
(keymap-set acm-mode-map "C-k" 'acm-select-prev)

;;DRAG
(keymap-set evil-normal-state-map "M-k" 'drag-stuff-up)
(keymap-set evil-normal-state-map "M-j" 'drag-stuff-down)
(keymap-set evil-normal-state-map "M-h" 'drag-stuff-left)
(keymap-set evil-normal-state-map "M-l" 'drag-stuff-right)
(keymap-set evil-normal-state-map "K" 'eldoc-box-help-at-point)

;;LSP
(keymap-set evil-normal-state-map "K" 'eldoc-box-help-at-point)

(defun hover-enable ()
  "Hover to provide details."
  (interactive)
  (global-eldoc-mode 1)
  )

(defun hover-disable ()
  "Hover to provide details."
  (interactive)
  (global-eldoc-mode 0)
  ;; (eldoc-box-hover-at-point-mode 0)
  )


;; GNRL
(use-package general
  :after evil
  :config

  (general-create-definer e/leader-keys
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC")

  (e/leader-keys
    "SPC" '(execute-extended-command :which-key "  M-x"))

  (e/leader-keys
    "f"  '(:ignore t :which-key "󰈔 files")
    "ff" '(find-file :which-key "󰈞 find a file")
    "fr" '(consult-recent-file :which-key "󰣜 recent files")
    "fc"  '(:ignore t :which-key "󰈔 current file")
    "fcs" '(crux-sudo-edit :which-key "󱄛 open this file as sudo")
    "fcr" '(crux-rename-buffer-and-file :which-key "󰑕 rename this file"))

  (e/leader-keys
    "o"  '(:ignore t :which-key "󰷏 open")
    "ow" '(crux-open-with :which-key " open with"))

  (e/leader-keys
    "b"  '(:ignore t :which-key " buffer navigation")
    "bd" '(kill-buffer-and-window :which-key "󰆴 kill the current buffer and window")
    "bk" '(crux-kill-other-buffers :which-key "󰛌 kill all other buffers and windows")
    "bn" '(next-buffer :which-key "󰛂 switch buffer")
    "bp" '(previous-buffer :which-key "󰛁 switch buffer")
    "bb" '(consult-buffer :which-key "󰕰 view buffers"))

  (e/leader-keys
    "s"  '(:ignore t :which-key " search")
    "ss" '(consult-line :which-key "󰱼 line search")
    "si" '(nerd-icons-insert :which-key "󰭟 search for icons")
    "srg" '(consult-ripgrep :which-key "󰟥 search with rg")
    "sd" '(dictionary-search :which-key "󰬋 search in dictionary"))

  (e/leader-keys
    "e"  '(:ignore t :which-key "󰈈  evaluate")
    "eb" '(eval-buffer :which-key "󰷊  evaluate buffer")
    "ear" '(crux-eval-and-replace :which-key "󰛔 evaluate and replace")
    "er" '(eval-region :which-key "󰨺  evaluate region"))

  (e/leader-keys
    "g"  '(:ignore :which-key "󰇐 goto")
    "gd"  '(lsp-bridge-find-def :which-key "󰇐 goto definitions")
    "gr"  '(lsp-bridge-find-references :which-key "󰇐 goto references"))

  (e/leader-keys
    "c"  '(:ignore t :which-key "󰅱 code")
    "cr"  '(lsp-bridge-rename :which-key "󰑕 rename")
    "cc"  '(crux-cleanup-buffer-or-region :which-key " cleanup")
    "cd"  '(crux-duplicate-current-line-or-region :which-key "󰬸 duplicate")
    "ch"  '(:ignore t :which-key "󰆽 hover")
    "che"  '(hover-enable :which-key " enable hover")
    "chd"  '(hover-disable :which-key " disable hover"))

  (e/leader-keys
    "h"  '(:ignore t :which-key "󰞋 help")
    "hF" '(describe-face :which-key "󱗎  describe face")
    "hf" '(describe-function :which-key "󰯻 describe function")
    "hb" '(embark-bindings :which-key "󰯻 describe bindings")
    "hv" '(describe-variable :which-key " describe variable")
    "hr"  '(:ignore t :which-key "󱍸 reload")
    "hrb" '(revert-buffer-quick :which-key "󰄸 reload buffer")
    "hrr" '((lambda () (interactive) (load-file "~/.config/emacs/init.el")) :wk "  Reload emacs config"))

  (e/leader-keys
    "t"  '(:ignore t :which-key " toggles")
    "tp"  '(perfect-margin-mode :which-key "  perfect margin mode toggle")
    "tv"  '(visual-line-mode :which-key "󰖶 toggle visual line mode")
    "tm"  '(minimap-mode :which-key "󰍍 minimap toggles"))
  )
;; Globals
;; (global-set-key (kbd "M-b") 'popper-toggle)
;; (global-set-key (kbd "M-n") 'popper-cycle)
;; (global-set-key (kbd "M-B") 'popper-toggle-type)
;; (global-set-key (kbd "M-q") 'popper-kill-latest-popup)

;;Embark
;; (define-key embark-file-map     (kbd "o") (my/embark-ace-action find-file))
;; (define-key embark-buffer-map   (kbd "o") (my/embark-ace-action switch-to-buffer))
;; (define-key embark-bookmark-map (kbd "o") (my/embark-ace-action bookmark-jump))

(provide 'keymaps)
