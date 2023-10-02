(use-package rainbow-delimiters :hook (prog-mode . rainbow-delimiters-mode))

;; (require 'lsp-bridge)
;; (global-lsp-bridge-mode)
(yas-global-mode 1)

(corfu-mode 1)
(add-hook 'prog-mode-hook 'eglot-ensure)
;; (with-eval-after-load 'eglot
;;   (add-to-list 'eglot-server-programs
;;                '(python-mode . ("pyright"))))
;; Diagnostics

;; (add-hook 'eglot-server-initialized-hook #'global-flycheck-mode)
;; (add-hook 'eglot-server-initialized-hook #'global-corfu-mode)
;; (add-hook 'eglot-server-initialized-hook #'flycheck-mode)
;; (add-hook 'eglot-server-initialized-hook #'corfu-mode)
(global-corfu-mode 1)
(global-flycheck-mode 1)

(setq eglot-ignored-server-capabilities '(:documentHighlightProvider))
(require 'eglot-flycheck-adapter)

(add-hook 'flycheck-mode-hook #'flycheck-posframe-mode)
(setq flycheck-border-width 20)


;; Enable auto completion and configure quitting
(setq corfu-auto t corfu-quit-no-match 'separator) ;; or t
(setq corfu-auto-delay 0)
(setq corfu-auto-prefix 1)

(setq completion-styles '(orderless basic)
      completion-category-defaults nil
      completion-category-overrides '((file (styles partial-completion))))



;; PAIRS
(electric-pair-mode 1)       ;; Turns on automatic parens pairing
(global-evil-matchit-mode 1)


;; Syntax-highlighting
;; there is ts mode baked in now
;; (setq font-lock-maximum-decoration t)
(require 'treesit)

(add-to-list 'treesit-language-source-alist '(bash "https://github.com/tree-sitter/tree-sitter-bash.git"))
(add-to-list 'treesit-language-source-alist '(python "https://github.com/tree-sitter/tree-sitter-python.git"))
(add-to-list 'treesit-language-source-alist '(css "https://github.com/tree-sitter/tree-sitter-css.git"))
(add-to-list 'treesit-language-source-alist '(html "https://github.com/tree-sitter/tree-sitter-html.git"))

(add-to-list 'major-mode-remap-alist '(c-mode . c-ts-mode))
(add-to-list 'major-mode-remap-alist '(c++-mode . c++-ts-mode))
(add-to-list 'major-mode-remap-alist '(sh-mode . bash-ts-mode))
(add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode))
(add-to-list 'major-mode-remap-alist '(css-mode . css-ts-mode))
(add-to-list 'major-mode-remap-alist '(shell-script-mode . bash-ts-mode))

(setq treesit-font-lock-level 4)
;; (add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode))
    ;; '(sh-mode . bash-ts-mode)
    ;; '(python-mode . python-ts-mode)
    ;; '(c++-mode . c++-ts-mode)
    ;; '(css-mode . css-ts-mode)
    ;; '(html-mode . html-ts-mode))

(provide 'codingEnhancement)
;;; codingEnhancement.el ends here

