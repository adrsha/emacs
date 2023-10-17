
;; ░█████╗░░█████╗░██████╗░██╗███╗░░██╗░██████╗░
;; ██╔══██╗██╔══██╗██╔══██╗██║████╗░██║██╔════╝░
;; ██║░░╚═╝██║░░██║██║░░██║██║██╔██╗██║██║░░██╗░
;; ██║░░██╗██║░░██║██║░░██║██║██║╚████║██║░░╚██╗
;; ╚█████╔╝╚█████╔╝██████╔╝██║██║░╚███║╚██████╔╝
;; ░╚════╝░░╚════╝░╚═════╝░╚═╝╚═╝░░╚══╝░╚═════╝░
;;
;;


;; LSP config
(setq eglot-sync-connect 0)
(require 'eglot-flycheck-adapter)


;; Corfu
(setq corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
(setq corfu-auto t)                 ;; Enable auto completion
(setq corfu-separator ?\s)          ;; Orderless field separator
(setq corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
(setq corfu-quit-no-match t)      ;; Never quit, even if there is no match
(setq corfu-preview-current nil)    ;; Disable current candidate preview
(setq corfu-preselect 'first)      ;; Preselect the prompt
(setq corfu-on-exact-match nil)     ;; Configure handling of exact matches
(setq corfu-scroll-margin 5)        ;; Use scroll margin
(setq corfu-minimum-width 100)        ;; Use scroll margin
(setq corfu-maximum-width 190)        ;; Use scroll margin
(setq corfu-auto-prefix 0.5)

;; (setq corfu--preview-ov nil)

;; Add extensions
(use-package cape
  ;; Bind dedicated completion commands
  ;; Alternative prefix keys: C-c p, M-p, M-+, ...
  :bind (("C-c p p" . completion-at-point) ;; capf
         ("C-c p t" . complete-tag)        ;; etags
         ("C-c p d" . cape-dabbrev)        ;; or dabbrev-completion
         ("C-c p h" . cape-history)
         ("C-c p f" . cape-file)
         ("C-c p k" . cape-keyword)
         ("C-c p s" . cape-symbol)
         ("C-c p a" . cape-abbrev)
         ("C-c p i" . cape-ispell)
         ("C-c p l" . cape-line)
         ("C-c p w" . cape-dict))
  :init
  
  ;; Add `completion-at-point-functions', used by `completion-at-point'.
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  ;;(add-to-list 'completion-at-point-functions #'cape-history)
  (add-to-list 'completion-at-point-functions #'cape-keyword)
  (add-to-list 'completion-at-point-functions #'cape-symbol)
  ;; (add-to-list 'completion-at-point-functions #'cape-line)
  )

(add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter)
(setq corfu-auto-delay 0.0)
(setq corfu-popupinfo-delay 0.3)
(add-hook 'prog-mode-hook (lambda () (interactive) (corfu-mode 1)))
(corfu-popupinfo-mode 1)
(corfu-history-mode 1)


;; For ESHELL
(add-hook 'eshell-mode-hook
          (lambda ()
            (setq-local corfu-auto nil)
            (corfu-mode)))


;; Flycheck diagnosis
(add-hook 'prog-mode-hook '(lambda () (interactive)
							 (add-hook 'evil-insert-state-exit-hook '(lambda () (interactive) (flycheck-mode 1)))
							 (add-hook 'evil-insert-state-entry-hook '(lambda () (interactive) (flycheck-mode -1)))
							 ))


;; EGLOT
(setq read-process-output-max (* 1024 1024))
(add-to-list 'eglot-server-programs '((c++-ts-mode c-ts-mode) "clangd"))
(add-to-list 'eglot-server-programs '(python-ts-mode . ("pyright-langserver" "--stdio")))

;; :completionProvider
;; :documentSymbolProvider
;; :codeLensProvider
;; :renameProvider
;; :inlayHintProvider
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


;; Options
(setq-default acm-backend-search-file-words-enable-fuzzy-match t)
;; (setq-default lsp-bridge-enable-org-babel t)
;; (setq-default lsp-bridge-symbols-enable-which-func t)
;; (setq-default lsp-bridge-enable-signature-help t)
;; (setq-default lsp-bridge-enable-diagnostics t)
;; (setq-default lsp-bridge-enable-hover-diagnostic t)

;; Docs
(setq counsel-describe-function-function #'helpful-callable)
(setq counsel-describe-variable-function #'helpful-variable)

;; Pairings
(electric-pair-mode 1)       ;; Turns on automatic parens pairing
(global-evil-matchit-mode 1)

;; Syntax Highlighting
(require 'treesit)

(add-to-list 'treesit-language-source-alist '(bash "https://github.com/tree-sitter/tree-sitter-bash.git"))
(add-to-list 'major-mode-remap-alist '(sh-mode . bash-ts-mode))
(add-to-list 'major-mode-remap-alist '(shell-script-mode . bash-ts-mode))

(add-to-list 'treesit-language-source-alist '(python "https://github.com/tree-sitter/tree-sitter-python.git"))
(add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode))

(add-to-list 'treesit-language-source-alist '(cpp "https://github.com/tree-sitter/tree-sitter-cpp.git"))
(add-to-list 'major-mode-remap-alist '(c++-mode . c++-ts-mode))

(add-to-list 'treesit-language-source-alist '(c "https://github.com/tree-sitter/tree-sitter-c"))
(add-to-list 'major-mode-remap-alist '(c-mode . c-ts-mode))

(add-to-list 'treesit-language-source-alist '(css "https://github.com/tree-sitter/tree-sitter-css.git"))
(add-to-list 'major-mode-remap-alist '(css-mode . css-ts-mode))

(add-to-list 'treesit-language-source-alist '(html "https://github.com/tree-sitter/tree-sitter-html.git"))
(add-to-list 'major-mode-remap-alist '(html-mode . html-ts-mode))

(setq treesit-font-lock-level 4)

;; Completions
(setq completion-category-defaults nil)
(setq completion-cycle-threshold 0)

;; (defun org-completion-symbols ()
;;   (when (looking-back "=[a-zA-Z]+")
;;     (let (cands)
;; 	  (save-match-data
;;         (save-excursion
;; 		  (goto-char (point-min))
;; 		  (while (re-search-forward "=\\([a-zA-Z]+\\)=" nil t)
;;             (cl-pushnew
;;              (match-string-no-properties 0) cands :test 'equal))
;; 		  cands))
;; 	  (when cands
;;         (list (match-beginning 0) (match-end 0) cands)))))

;; (defun org-cap-filesystem ()
;;   (let (path)
;;     (when (setq path (ffap-string-at-point))
;; 	  (when (string-match "\\`file:\\(.*\\)\\'" path)
;;         (setq path (match-string 1 path)))
;; 	  (let ((compl (all-completions path #'read-file-name-internal)))
;;         (when compl
;; 		  (let* ((str (car compl))
;;                  (offset
;; 				  (let ((i 0)
;;                         (len (length str)))
;;                     (while (and (< i len)
;;                                 (equal (get-text-property i 'face str)
;; 									   'completions-common-part))
;; 					  (cl-incf i))
;;                     i)))
;;             (list (- (point) offset) (point) compl)))))))

;; (setq-default completion-at-point-functions '(org-completion-symbols org-cap-filesystem elisp-completion-at-point t))

;; Open in vim

(use-package flycheck
  :preface

  (defun mp-flycheck-eldoc (callback &rest _ignored)
    "Print flycheck messages at point by calling CALLBACK."
    (when-let ((flycheck-errors (and flycheck-mode (flycheck-overlay-errors-at (point)))))
      (mapc
       (lambda (err)
         (funcall callback
				  (format "%s %s\n"
						  (let ((level (flycheck-error-level err)))
							(pcase level
							  ('info (propertize (concat "  " "Info. \n")
												 'face 'flycheck-error-list-info))
							  ('error (propertize (concat "󰚌  " "Error! \n")
												  'face 'flycheck-error-list-error))
							  ('warning (propertize (concat "  " "Warning! \n")
													'face 'flycheck-error-list-warning))
							  (_ level)))
						  (flycheck-error-message err)
						  )
				  :thing (or (flycheck-error-id err)
							 (flycheck-error-group err))
				  :face 'font-lock-doc-face))
       flycheck-errors)))

  (defun mp-flycheck-prefer-eldoc ()
    (add-hook 'eldoc-documentation-functions #'mp-flycheck-eldoc nil t)
    (setq eldoc-documentation-strategy 'eldoc-documentation-compose-eagerly)
    (setq flycheck-display-errors-function nil)
    (setq flycheck-help-echo-function nil))

  (use-package eglot
	:preface
	(defun mp-eglot-eldoc ()
      (setq eldoc-documentation-strategy
            'eldoc-documentation-compose-eagerly))
	:hook ((eglot-managed-mode . mp-eglot-eldoc)))

  :hook ((flycheck-mode . mp-flycheck-prefer-eldoc)))

(defun minimal-code-enable ()
  (interactive)
  (echo-bar-mode -1)
  (breadcrumb-mode -1)
  )

(defun minimal-code-disable ()
  (interactive)
  (echo-bar-mode 1)
  (breadcrumb-mode 1)
  )
(provide 'codingEnhancement)
      ;;; codingEnhancement.el ends here
