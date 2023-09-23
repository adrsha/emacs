;; NOTE: init.el is now generated from Emacs.org.  Please edit that file
;;       in Emacs and init.el will be generated automatically!
(setq inhibit-startup-message t) ;; Landing page in EMACS
(scroll-bar-mode -1) ;; Disables scroll-bars
(tool-bar-mode -1) ;; Disables the top toolbars
(menu-bar-mode -1) ;; Disable the menu bar
;; Centralize Backup
(setq backup-directory-alist '((".*" . "~/.local/share/Trash/files")))

;; Setting the fonts
(set-face-attribute 'default nil :font "JetBrainsMono Nerd Font Medium" :height 130)
(set-face-attribute 'fixed-pitch nil :font "JetBrainsMono Nerd Font Medium" :height 130)
;; (set-face-attribute 'variable-pitch nil :font "Iosevka Aile" :height 140 :weight 'regular)
(set-face-attribute 'variable-pitch nil :font "Quicksand" :height 140 :weight 'medium)
(set-face-attribute 'font-lock-comment-face nil :slant 'italic)
(set-face-attribute 'font-lock-keyword-face nil :slant 'italic)

(custom-set-faces
 '(header-line
   ((t (:background "#11111b"))))
 )

(defun chilly/org-font-setup ()
  ;; Replace list hyphen with dot
  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

  ;; Set faces for heading levels
  (dolist (face '((org-meta-line . 0.5)
                  (org-level-1 . 1.7)
                  (org-level-2 . 1.5)
                  (org-level-3 . 1.4)
                  (org-level-4 . 1.3)
                  (org-level-5 . 1.3)
                  (org-level-6 . 1.3)
                  (org-level-7 . 1.2)
                  (org-level-8 . 1.1)))
    ;; (set-face-attribute (car face) nil :font "ETbb" :weight 'regular :height (cdr face))
    (set-face-attribute (car face) nil :font "Rubik" :weight 'regular :height (cdr face))
    )
  (set-face-attribute 'line-number nil :font "JetBrainsMono Nerd Font Bold" :height 90)

  ;; Ensure that anything that should be fixed-pitch in Org files appears that way
  (set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-table nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch))

;; Initialize Package souces
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package) ;; sth-p in emacs is mostly a function that takes t or nil
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t) ;; Default is nil

(use-package catppuccin-theme
  :init
  (setq catppuccin-flavor 'mocha)
  (setq catppuccin-italic-comments t)
  (setq catppuccin-italic-blockquotes t)
  :config
  (catppuccin-set-color 'base "#11111b")
  (catppuccin-set-color 'mantle "#0B0B11")
  (catppuccin-reload))

(use-package all-the-icons)

(use-package dashboard
  :ensure t 
  :init
  (setq initial-buffer-choice 'dashboard-open)
  (setq dashboard-set-heading-icons nil)
  (setq dashboard-set-file-icons t)
  (setq dashboard-banner-logo-title "You better crush it now.")
  (setq dashboard-show-shortcuts t)
  (setq dashboard-startup-banner "/home/chilly/.config/emacs/images/emacs-dash.png")  ;; use custom image as banner
  (setq dashboard-center-content t) ;; set to 't' for centered content
  (setq dashboard-items '((recents . 3)
                          (projects . 3)))
  :custom
  (dashboard-modify-heading-icons '((recents . "file-text")
                                    (bookmarks . "book")))

  :if (< (length command-line-args) 3)
  :config
  (dashboard-setup-startup-hook))

(use-package hl-todo
  :hook ((org-mode . hl-todo-mode)
         (prog-mode . hl-todo-mode))
  :config
  (setq hl-todo-highlight-punctuation ":"
        hl-todo-keyword-faces
        `(("TODO"       warning bold)
          ("FIXME"      error bold)
          ("HACK"       font-lock-constant-face bold)
          ("REVIEW"     font-lock-keyword-face bold)
          ("NOTE"       success bold)
          ("DEPRECATED" font-lock-doc-face bold))))

(setq ivy-ignore-buffers '("\*.*\*"))
(use-package swiper :ensure t)

(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)	
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1))

(use-package counsel
  :bind (("M-x" . counsel-M-x)
         ("C-x b" . counsel-ibuffer)
         ("C-x f" . counsel-find-file)))
;; add descriptions for commands in counsel mode
(use-package ivy-rich
  :init
  (ivy-rich-mode 1))


;; Enable vertico
(use-package vertico
  :init
  (vertico-mode)

  ;; Different scroll margin

  ;; (setq vertico-scroll-margin 0)

  ;; Show more candidates
  ;; (setq vertico-count 20)

  ;; Grow and shrink the Vertico minibuffer
  ;; (setq vertico-resize t)

  ;; Optionally enable cycling for `vertico-next' and `vertico-previous'.
  ;; (setq vertico-cycle t)
  )

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :init
  (savehist-mode))

;; A few more useful configurations...
(use-package emacs
  :init
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Emacs 28: Hide commands in M-x which do not work in the current mode.
  ;; Vertico commands are hidden in normal buffers.
  ;; (setq read-extended-command-predicate
  ;;       #'command-completion-default-include-p)

  ;; Enable recursive minibuffers
  (setq enable-recursive-minibuffers t))


(use-package consult)

(use-package undo-tree
  :ensure t
  :after evil
  :diminish
  :config
  (evil-set-undo-system 'undo-tree)
  (global-undo-tree-mode 1))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  ;; (setq which-key-popup-type 'minibuffer)
  ;;(setq which-key-popup-type 'side-window)
  (setq which-key-idle-secondary-delay 0)
  (setq which-key-idle-delay 0))

(use-package perfect-margin
  :init (perfect-margin-mode))

(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

(electric-pair-mode 1)       ;; Turns on automatic parens pairing

;; Custom functions
(defvar org-electric-pairs '((?/ . ?/)) "Electric pairs for org-mode.")
(defun org-add-electric-pairs ()
  (setq-local electric-pair-pairs (append electric-pair-pairs org-electric-pairs))
  (setq-local electric-pair-text-pairs electric-pair-pairs))

(add-hook 'org-mode-hook 'org-add-electric-pairs)

;; Disable the autocompletion of pairs <>
(add-hook 'org-mode-hook (lambda ()
                           (setq-local electric-pair-inhibit-predicate
                                       `(lambda (c)
                                          (if (char-equal c ?<) t (,electric-pair-inhibit-predicate c))))))

;; Dependency to lsp-bridge
(use-package yasnippet)
(yas-global-mode 1)
(use-package markdown-mode
  :ensure t
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown")
  :bind (:map markdown-mode-map
              ("C-c C-e" . markdown-do)))

(add-to-list 'load-path "~/.config/emacs/scripts/lsp-bridge/")

(require 'lsp-bridge)
(global-lsp-bridge-mode)

(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy))
  :init
  ;; NOTE: Set this to the folder where you keep your Git repos!
  (when (file-directory-p "~/Code")
    (setq projectile-project-search-path '("~/Code")))
  (setq projectile-switch-project-action #'projectile-dired))

(use-package counsel-projectile
  :after projectile
  :config (counsel-projectile-mode))

;; KEYMAPS
(use-package general
  :after evil
  :config

  (general-create-definer e/leader-keys
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC")

  (e/leader-keys
    "SPC" '(counsel-M-x :which-key "M-x"))

  (e/leader-keys
    "f"  '(:ignore t :which-key "files")
    "ff" '(find-file :which-key "recent files")
    "fr" '(counsel-recentf :which-key "recent files"))

  (e/leader-keys
    "b"  '(:ignore t :which-key "buffer navigation")
    "bd" '(kill-buffer-and-window :which-key "kill the current buffer and window")
    "bn" '(next-buffer :which-key "󰛂 switch buffer")
    "bp" '(previous-buffer :which-key "󰛁 switch buffer")
    "bb" '(counsel-switch-buffer :which-key "view buffers"))  

  (e/leader-keys
    "s"  '(:ignore t :which-key "search")
    "ss" '(:ignore t :which-key "swiper")
    "sso" '(counsel-grep-or-swiper :which-key "open swiper")
    "ssh" '(swiper-thing-at-point :which-key "swipe this point")
    "ssw" '(swiper-thing-at-point :which-key "swipe for word")
    "ssm" '(swiper-all :which-key "swipe on other buffers")
    "sr" '(eval-buffer :which-key "open swiper")
    "sd" '(eval-region :which-key "search in dictionary"))

  (e/leader-keys
    "e"  '(:ignore t :which-key "evaluate")
    "eb" '(eval-buffer :which-key "evaluate buffer")
    "er" '(eval-region :which-key "evaluate region"))

  (e/leader-keys
    "h"  '(:ignore t :which-key "help")
    "hF" '(describe-face :which-key "describe face")
    "hf" '(describe-function :which-key "describe function")
    "hv" '(describe-variable :which-key "describe variable")
    "hr"  '(:ignore t :which-key "reload")
    "hrb" '(revert-buffer-quick :which-key "reload buffer")
    "hrr" '((lambda () (interactive)
              (load-file "~/.config/emacs/init.el")
              (ignore (elpaca-process-queues)))
            :wk "Reload emacs config"))

  (e/leader-keys
    "p"  '(projectile-command-map :which-key "project"))

  (e/leader-keys
    "t"  '(:ignore t :which-key "toggles")
    "tp"  '(perfect-margin-mode :which-key "perfect margin mode toggle")
    "tv"  '(visual-line-mode :which-key "toggle visual line mode")
    "tm"  '(minimap-mode :which-key "minimap toggles"))
  )

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-d-scroll t)
  :config
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)


  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package evil-escape
  :after evil
  :init
  (evil-escape-mode 1)
  :config
  (setq-default evil-escape-key-sequence "jk"
                evil-escape-delay 0.3))

(use-package evil-commentary
  :after evil
  :init
  (evil-commentary-mode 1)
  :config)

;; Using RETURN to follow links in Org/Evil 
;; Unmap keys in 'evil-maps if not done, (setq org-return-follows-link t) will not work
(with-eval-after-load 'evil-maps
  (define-key evil-motion-state-map (kbd "SPC") nil)
  (define-key evil-motion-state-map (kbd "RET") nil)
  (define-key evil-motion-state-map (kbd "TAB") nil))

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
;; (define-key prog-mode-map (kbd "C-x M-t") 'counsel-load-theme)

(define-key evil-motion-state-map (kbd "C-h") 'nil)
(define-key global-map (kbd "C-h") #'nil)
(define-key evil-normal-state-map (kbd "C-h") 'nil)
(define-key evil-insert-state-map (kbd "C-h") 'nil)
(define-key acm-mode-map (kbd "C-h") #'nil)

(define-key evil-insert-state-map (kbd "C-j") 'nil)
(define-key evil-insert-state-map (kbd "C-k") 'nil)
(define-key evil-insert-state-map (kbd "C-l") 'acm-complete)

(define-key global-map (kbd "C-j") #'acm-select-next)
(define-key global-map (kbd "C-k") #'acm-select-prev)
(define-key global-map (kbd "C-l") #'acm-complete)

(define-key acm-mode-map (kbd "C-l") 'acm-complete)
(define-key acm-mode-map (kbd "C-j") 'acm-select-next)
(define-key acm-mode-map (kbd "C-k") 'acm-select-prev)

;; Setting RETURN key in org-mode to follow links
(setq org-return-follows-link  t)
;; (electric-indent-mode -1)    ;; Turn off the weird indenting that Emacs does by default.

(setq org-edit-src-content-indentation 2) ;; Set src block automatic indent to 0 instead of 2.
(setq org-ellipsis "")

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (python . t)))

(push '("conf-unix" . conf-unix) org-src-lang-modes)

(setq org-confirm-babel-evaluate nil)

;; Automatically tangle our Emacs.org config file when we save it
(defun chilly/org-babel-tangle-config ()
  (when (string-equal (buffer-file-name)
                      (expand-file-name "~/.config/emacs/Emacs.org"))
    ;; Dynamic scoping to the rescue
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))

(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'chilly/org-babel-tangle-config)))

(defun chilly/org-mode-setup ()
  (org-indent-mode)
  ;; (variable-pitch-mode 1) ;; Makes things much slower in org mode
  )

(use-package org
  :hook (org-mode . chilly/org-mode-setup)
  :config
  (setq org-ellipsis " ▾")
  (chilly/org-font-setup))

(require 'org-tempo)

(add-to-list 'org-structure-template-alist '("sh" . "src shell"))
(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
(add-to-list 'org-structure-template-alist '("py" . "src python"))

;; Only warn about important stuff
(setq warning-minimum-level :emergency)

;; Disable the modeline itself
(setq-default mode-line-format nil) 

;; Get rid of the extra buffers
(setq-default message-log-max nil)

;; Makes *scratch* empty.
(setq initial-scratch-message "")

;; Removes *scratch* from buffer after the mode has been set.
(defun remove-scratch-buffer ()
  (if (get-buffer "*scratch*")
      (kill-buffer "*scratch*")))
(add-hook 'after-change-major-mode-hook 'remove-scratch-buffer)

;; Removes *messages* from the buffer.
(setq-default message-log-max nil)
(kill-buffer "*Messages*")

;; Removes *Completions* from buffer after you've opened a file.
(add-hook 'minibuffer-exit-hook
          '(lambda ()
             (let ((buffer "*Completions*"))
               (and (get-buffer buffer)
                    (kill-buffer buffer)))))

;; Don't show *Buffer list* when opening multiple files at the same time.
(setq inhibit-startup-buffer-menu t)

;; Show only one active window when opening multiple files at the same time.
(add-hook 'window-setup-hook 'delete-other-windows)

;; To add the scripts  
(add-to-list 'load-path "~/.config/emacs/scripts/")

;;MINIMAP
(require 'minimap)
(setq
 ;; Configure minimap position
 minimap-window-location 'right ; Minimap on the right side
 minimap-width-fraction 0.0 ; slightly smaller minimap
 minimap-minimum-width 20 ; also slightly smaller minimap
 minimap-maximum-width 30 ; also slightly smaller minimap

 minimap-dedicated-window t ; seems to work better
 minimap-enlarge-certain-faces nil ; enlarge breaks BlockFont
 )

;; ORG-BULLETS

(require 'org-bullets)
(setq org-bullets-bullet-list '("⟶ " "⟶ " "⟶ " "⟶ " "⟶ " "⟶ "))
;; (setq org-bullets-bullet-list '(" " " " " " " " " " " "))
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
(font-lock-add-keywords 'org-mode
                        '(("^ *\\([-]\\) "
                           (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))
(setq org-hide-emphasis-markers t)

;; prevent number lines to show in terminals NOTE: Removed org-mode-hook
(dolist (mode '(term-mode-hook
                shell-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))


(column-number-mode)
(global-display-line-numbers-mode t)
(auto-fill-mode 1)	     ;; Make wrap actually break line
(auto-revert-mode 1)	     ;; Revert the buffer automatically

(set-frame-parameter (selected-frame) 'buffer-predicate
                     (lambda (buf) (not (string-match-p "^\*.*\*" (buffer-name buf)))))

;; DONT TOUCH THIS:
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(hl-todo minimap org-modern counsel-projectile projectile hydra all-the-icons helpful perfect-margin which-key evil-commentary evil-escape evil-collection evil general rainbow-delimiters ivy-rich counsel swiper catppuccin-theme)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
