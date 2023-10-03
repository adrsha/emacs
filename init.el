;; -*- lexical-binding: t -*-
;; One path is essential to load the place for the default-paths folder
(add-to-list 'load-path "~/.config/emacs/plugs/")
(require 'default-paths) 
(require 'cleanfiles) 

 
;; Install packages
(require 'usepackage)
(use-package catppuccin-theme)
(use-package which-key)
(use-package rainbow-delimiters)

(use-package undo-fu-session)
(use-package helpful)

(use-package pulsar)
(use-package posframe)
(use-package consult)
(use-package consult-eglot)
(use-package embark-consult)
(use-package embark)
(use-package vertico)
(use-package orderless)
(use-package marginalia)

(use-package treemacs)
(use-package treemacs-evil)
(use-package treemacs-projectile)
(use-package treemacs-icons-dired)
(use-package treemacs-tab-bar) 

(use-package evil)
(use-package evil-collection)
(use-package evil-escape)
(use-package evil-matchit)
(use-package evil-surround)
(use-package evil-commentary)
(use-package general)

(use-package yasnippet)
(use-package yasnippet-snippets)
(use-package markdown-mode)
(use-package format-all)

(use-package org-bullets)
(use-package drag-stuff)
(use-package crux)

(use-package corfu)
(use-package mono-complete)
(use-package flycheck)
(use-package flycheck-posframe)
(use-package kind-icon)
(use-package projectile)
(use-package nerd-icons)
(use-package nerd-icons-completion)
(use-package ligature)

(use-package hl-todo)
(use-package rainbow-mode)
(elpaca-wait)

;; NOTE: Configs

(require 'colorscheme)
(require 'fonts)
(require 'options)

(require 'vertico-setup)
(require 'popupmenu)

(require 'codingEnhancement)
(require 'dataManagers)
(require 'evil-remaps)
(require 'keymaps)
(require 'whichkey-setup)

(require 'prettify-utils)
(require 'org-setup)
(require 'uiChanges)

(require 'explain-pause-mode)
