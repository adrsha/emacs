;; Make ESC quit prompts

(use-package crux)
(use-package drag-stuff)

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; (define-key vertico-map (kbd "TAB") 'vertico-next)
(keymap-set vertico-map "RET" '(lambda () (interactive) (vertico-insert) (minibuffer-force-complete-and-exit)))
;; (keymap-set vertico-map "C-l" '(lambda () (interactive) (vertico-insert) (minibuffer-force-complete-and-exit)))
(keymap-set vertico-map "C-l" #'vertico-insert)
(keymap-set vertico-map "C-j" #'vertico-next)
(keymap-set vertico-map "C-k" #'vertico-previous)
(keymap-set vertico-map "C-h" #'vertico-directory-up)

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

(keymap-set evil-normal-state-map "K" '(lambda () (interactive) (global-eldoc-mode 1) (eldoc-box-hover-at-point-mode 1)))
;; (keymap-set evil-normal-state-map "K" 'eldoc-box-help-at-point)

;;LSP
(defun hover-enable ()
  "Hover to provide details."
  (interactive)
  (global-eldoc-mode 1)
  (eldoc-box-hover-at-point-mode 1))

(defun hover-disable ()
  "Hover to provide details."
  (interactive)
  (global-eldoc-mode 0)
  (eldoc-box-hover-at-point-mode 0))


;; GNRL
(use-package general
  :after evil
  :config

  (general-create-definer e/leader-keys
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC")

  (e/leader-keys
    "SPC" '(execute-extended-command :which-key " M-x"))

  (e/leader-keys
    "f"  '(:ignore t :which-key "󰈔 files")
    "ff" '(find-file :which-key "󰈞 find a file")
    "fr" '(consult-recent-file :which-key "󰣜 recent files"))

  (e/leader-keys
    "b"  '(:ignore t :which-key "󱠿 buffer navigation")
    "bd" '(kill-buffer-and-window :which-key "󰆴 kill the current buffer and window")
    "bn" '(next-buffer :which-key "󰛂 switch buffer")
    "bp" '(previous-buffer :which-key "󰛁 switch buffer")
    "bb" '(consult-buffer :which-key "󰕰 view buffers"))  

  (e/leader-keys
    "s"  '(:ignore t :which-key " search")
    "ss" '(consult-line :which-key "󰱼 line search")
    "si" '(nerd-icons-insert :which-key "󰭟 search for icons")
    "sd" '(dictionary-search :which-key "󰬋 search in dictionary"))

  (e/leader-keys
    "e"  '(:ignore t :which-key "󰈈  evaluate")
    "eb" '(eval-buffer :which-key "󰷊  evaluate buffer")
    "er" '(eval-region :which-key "󰨺  evaluate region"))

  (e/leader-keys
    "c"  '(:ignore t :which-key "󰅱 code")
    "ch"  '(:ignore t :which-key "󰅱 hover")
    "che"  '(hover-enable :which-key "enable hover")
    "chd"  '(hover-disable :which-key "disable hover")
    "cf" '(format-all-region-or-buffer :which-key "󰉼 format the code"))


  (e/leader-keys
    "h"  '(:ignore t :which-key "󰞋 help")
    "hF" '(describe-face :which-key "󱗎  describe face")
    "hf" '(describe-function :which-key "󰊕 describe function")
    "hv" '(describe-variable :which-key " describe variable")
    "hr"  '(:ignore t :which-key "󰑓 reload")
    "hrb" '(revert-buffer-quick :which-key "󰄸 reload buffer")
    "hrr" '((lambda () (interactive) (load-file "~/.config/emacs/init.el")) :wk "  Reload emacs config"))

  (e/leader-keys
    "t"  '(:ignore t :which-key "toggles")
    "tp"  '(perfect-margin-mode :which-key "perfect margin mode toggle")
    "tv"  '(visual-line-mode :which-key "toggle visual line mode")
    "tm"  '(minimap-mode :which-key "minimap toggles"))
)
;; Globals
(global-set-key (kbd "M-b") 'popper-toggle)  
(global-set-key (kbd "M-n") 'popper-cycle)
(global-set-key (kbd "M-B") 'popper-toggle-type)
(global-set-key (kbd "M-q") 'popper-kill-latest-popup)



(provide 'keymaps)
