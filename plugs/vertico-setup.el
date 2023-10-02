;; Preamble
(nerd-icons-completion-mode)
(add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup)
(savehist-mode)
(vertico-mode)
(setq vertico-scroll-margin 0)
(setq vertico-cycle t)

(setq completion-styles '(orderless basic substring partial-completion ))
(setq completion-category-overrides '((file (styles basic partial-completion))))

(add-hook 'completion-list-mode #'consult-preview-at-point-mode)
(setq register-preview-delay 0.5
      register-preview-function #'consult-register-format)
(advice-add #'register-preview :override #'consult-register-window)

(setq xref-show-xrefs-function #'consult-xref
      xref-show-definitions-function #'consult-xref)


(setq consult-async-min-input 1)
(setq consult-preview-key "C-h")
(add-to-list 'consult-buffer-filter "\*.*\*")

(consult-customize
 consult-ripgrep consult-git-grep consult-grep
 consult-bookmark consult-recent-file consult-xref
 consult--source-bookmark consult--source-file-register
 consult--source-recent-file consult--source-project-recent-file
 :preview-key "C-h")            ;; Option 2: Manual preview
(marginalia-mode)
(setq prefix-help-command #'embark-prefix-help-command)

;; options
(setq-default acm-backend-search-file-words-enable-fuzzy-match t)
(setq-default lsp-bridge-enable-org-babel t)
(setq-default lsp-bridge-symbols-enable-which-func t)
(setq-default lsp-bridge-enable-signature-help t)
(setq-default lsp-bridge-enable-diagnostics t)
(setq-default lsp-bridge-enable-hover-diagnostic t)

(provide 'vertico-setup)
