;; -*- lexical-binding: t -*-
(add-to-list 'load-path "~/.config/emacs/plugs/")
(add-to-list 'load-path "~/.config/emacs/plugs/use-package/")

(require 'cleanfiles) ;; set this here so that others use this default config
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
