
;; ░█████╗░░█████╗░██████╗░██╗███╗░░██╗░██████╗░
;; ██╔══██╗██╔══██╗██╔══██╗██║████╗░██║██╔════╝░
;; ██║░░╚═╝██║░░██║██║░░██║██║██╔██╗██║██║░░██╗░
;; ██║░░██╗██║░░██║██║░░██║██║██║╚████║██║░░╚██╗
;; ╚█████╔╝╚█████╔╝██████╔╝██║██║░╚███║╚██████╔╝
;; ░╚════╝░░╚════╝░╚═════╝░╚═╝╚═╝░░╚══╝░╚═════╝░
;;
;;

;; Preamble
(add-to-list 'load-path "~/.config/emacs/plugs/lsp-bridge/")

;; LSP config
;; (require 'lsp-bridge)
;; (global-lsp-bridge-mode)
(setq eglot-sync-connect 0)
(require 'eglot-flycheck-adapter)

(use-package corfu)
(setq corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
(setq corfu-auto nil)                 ;; Enable auto completion
(setq corfu-separator ?\s)          ;; Orderless field separator
(setq corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
(setq corfu-quit-no-match t)      ;; Never quit, even if there is no match
(setq corfu-preview-current nil)    ;; Disable current candidate preview
(setq corfu-preselect 'prompt)      ;; Preselect the prompt
(setq corfu-on-exact-match nil)     ;; Configure handling of exact matches
(setq corfu-scroll-margin 5)        ;; Use scroll margin
(setq corfu-minimum-width 100)        ;; Use scroll margin
(setq corfu-maximum-width 190)        ;; Use scroll margin
(setq corfu-auto-prefix 0)
(add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter)

(setq corfu-auto-delay 0.0)
(setq corfu-popupinfo-delay 0.0)
(global-corfu-mode 1)
(add-hook 'prog-mode-hook 'flycheck-mode)
(add-hook 'prog-mode-hook 'flycheck-posframe-mode)
(setq mono-complete-preview-delay 0.0)


;; Enable Corfu only for certain modes.
;; :hook ((prog-mode . corfu-mode)
;;        (shell-mode . corfu-mode)
;;        (eshell-mode . corfu-mode))


(setq completion-category-defaults nil)
(setq completion-cycle-threshold 0)

;; EGLOT
;; (setq read-process-output-max (* 1024 1024))
(add-to-list 'eglot-server-programs '((c++-ts-mode c-ts-mode) "clangd"))
(add-to-list 'eglot-server-programs '(python-ts-mode . ("pyright-langserver" "--stdio")))

;; :completionProvider
;; :documentSymbolProvider
;; :codeLensProvider
;; :renameProvider
(setq eglot-ignored-server-capabilites '(:documentHighlightProvider
                                         :workspaceSymbolProvider
                                         :documentFormattingProvider
                                         :documentRangeFormattingProvider
                                         :documentOnTypeFormattingProvider
                                         :foldingRangeProvider))

(add-hook 'prog-mode-hook 'eglot-ensure)
(add-hook 'eglot--managed-mode-hook (lambda () (flymake-mode -1)))
(setq flycheck-display-errors-delay 0.4)
(setq flycheck-cancel-error-display-at-point-timer 1.0)
;; (setq eglot-ignored-server-capabilites '(:inlayHintProvider))
;; (setq eglot-ignored-server-capabilites '(:inlayHintProvider))

;; Ghost text complete
;; (mono-complete-mode)
;; (setq mono-complete-preview-delay 0)

;; Options
(setq-default acm-backend-search-file-words-enable-fuzzy-match t)
(setq-default lsp-bridge-enable-org-babel t)
(setq-default lsp-bridge-symbols-enable-which-func t)
(setq-default lsp-bridge-enable-signature-help t)
(setq-default lsp-bridge-enable-diagnostics t)
(setq-default lsp-bridge-enable-hover-diagnostic t)

;; Docs
(setq counsel-describe-function-function #'helpful-callable)
(setq counsel-describe-variable-function #'helpful-variable)

;; Pairings
(electric-pair-mode 1)       ;; Turns on automatic parens pairing
(global-evil-matchit-mode 1)

;; Syntax Highlighting
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

(provide 'codingEnhancement)
      ;;; codingEnhancement.el ends here
