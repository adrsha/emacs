;; One path is essential to load the place for the default-paths folder
(add-to-list 'load-path "~/.config/emacs/plugs/")
(require 'default-paths)
(require 'startup)

;; Install packages
(require 'usepackage)
(use-package catppuccin-theme)

(use-package undo-fu-session)
(use-package helpful)

;; use-package with Elpaca:
(use-package dashboard)
(use-package pulsar)
(use-package posframe)
(use-package consult)
(use-package consult-eglot)
(use-package embark-consult)
(use-package embark)
(use-package vertico)
(use-package orderless)
(use-package marginalia)
(use-package yafolding)

(use-package evil)
(use-package evil-collection)
(use-package evil-commentary)
(use-package evil-escape)
(use-package evil-matchit)
(use-package evil-surround)
(use-package evil-mc)
(use-package general)

(use-package treemacs)
(use-package treemacs-evil)
(use-package treemacs-projectile)
(use-package treemacs-icons-dired)
(use-package yasnippet)
(use-package yasnippet-snippets)
(use-package markdown-mode)
(use-package format-all)
(use-package org-bullets)
(use-package org-roam)
(use-package drag-stuff)
(use-package crux)
(use-package visual-fill-column)
(use-package popper)


(use-package corfu)
(use-package cape)
(use-package flycheck)

(use-package flycheck-posframe)

(use-package focus)
(use-package kind-icon)
(use-package projectile)
(use-package nerd-icons)
(use-package nerd-icons-dired)
(use-package nerd-icons-completion)
(use-package ligature)
(use-package expand-region)
(use-package hl-todo)
(use-package rainbow-mode)
(use-package rainbow-delimiters)
(use-package which-key)
(use-package hydra)

(elpaca-wait)

;; NOTE: Configs

(require 'colorscheme)
(require 'options)

(require 'dashboard-setup)
(require 'snippets-setup)
(require 'whichkey-setup)
(require 'vertico-setup)
(require 'hydra-posframe)
(require 'popupmenu)

(require 'dataManagers)
(require 'codingEnhancement)
(require 'evil-remaps)
(require 'keymaps)


(require 'uiChanges)
(require 'org-setup)

(require 'prettify-utils)

(require 'explain-pause-mode)
(require 'expand-region)
(require 'hydras)

(put 'dired-find-alternate-file 'disabled nil)
