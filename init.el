;; -*- lexical-binding: t -*-
(add-to-list 'load-path "~/.config/emacs/plugs/")
(add-to-list 'load-path "~/.config/emacs/plugs/use-package/")


(require 'usepackage)

;; load the colorscheme first to prevent getting blinded accidentally
(require 'colorscheme)
(require 'fonts)

(require 'options)

(require 'undoredo)
(require 'vertico-setup)
(require 'popupmenu)
(require 'icons-setup)
(require 'betterDocs)

;; load the lsp and stuff first and then override the keymaps
(require 'codingEnhancement)
(require 'projectile-setup)
(require 'evil-remaps)
(require 'keymaps)
(require 'whichkey-setup)

(require 'prettify-utils)
(require 'org-setup)
(require 'uiChanges)

;; Clearn buffers after loading lsp and vertico/consult
(require 'cleanBuffers)

(eldoc-box-hover-at-point-mode)
;; WARN: Dont touch the follows

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(popper centaur-tabs lsp-ui lsp-mode good-scroll scroll-on-jump format-all drag-stuff helpful hl-todo which-key whichkey yasnippet-snippetj corfu yasnippet-snippets yasnippet rainbow-delimiters projectile minibuffer-header evil))
 '(warning-suppress-log-types
   '(((package reinitialization))
     ((package reinitialization))))
 '(warning-suppress-types '(((package reinitialization)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
