;; Syntax-highlighting
(setq font-lock-maximum-decoration t)

(use-package rainbow-delimiters
             :hook (prog-mode . rainbow-delimiters-mode))

;; Dependency to lsp-bridge
(use-package yasnippet)
(use-package yasnippet-snippets)
(yas-global-mode 1)


(use-package markdown-mode
  :ensure t
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown")
  :bind (:map markdown-mode-map
              ("C-c C-e" . markdown-do)))

(add-to-list 'load-path "~/.config/emacs/plugs/lsp-bridge/")

(require 'lsp-bridge)
(global-lsp-bridge-mode)

(setq-default acm-backend-search-file-words-enable-fuzzy-match t)
(setq-default acm-enable-doc-markdown-render 'async)
(setq-default lsp-bridge-enable-org-babel t)
(setq-default lsp-bridge-symbols-enable-which-func t)
(setq-default lsp-bridge-enable-signature-help t)
(setq-default lsp-bridge-enable-diagnostics t)
(setq-default lsp-bridge-enable-hover-diagnostic t)
(setq-default lsp-bridge-enable-completion-in-minibuffer t)


(set-face-attribute 'acm-frame-default-face nil :background "#0E0E16")
(set-face-attribute 'acm-frame-border-face nil :background "#0E0E16")
(set-face-attribute 'acm-frame-select-face nil :background "#11111B")
(set-face-attribute 'acm-filter-face nil :background "#0E0E16")


;; PAIRS
(electric-pair-mode 1)       ;; Turns on automatic parens pairing

;; Formatting

(use-package format-all)

(provide 'codingEnhancement)
