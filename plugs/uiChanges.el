;; ██╗░░░██╗██╗░░░░░░██╗░░░██╗████████╗██╗██╗░░░░░░██████╗
;; ██║░░░██║██║░░░░░░██║░░░██║╚══██╔══╝██║██║░░░░░██╔════╝
;; ██║░░░██║██║█████╗██║░░░██║░░░██║░░░██║██║░░░░░╚█████╗░
;; ██║░░░██║██║╚════╝██║░░░██║░░░██║░░░██║██║░░░░░░╚═══██╗
;; ╚██████╔╝██║░░░░░░╚██████╔╝░░░██║░░░██║███████╗██████╔╝
;; ░╚═════╝░╚═╝░░░░░░░╚═════╝░░░░╚═╝░░░╚═╝╚══════╝╚═════╝░

;; Custom Variables

(defvar bgcolor "#11111b"
  "The normal background of emacs.")
(defvar darker-bgcolor "#0D0D15"
  "The darker background of emacs.")
(defvar calm-fgcolor "#BAC2DE"
  "The calm foreground of emacs.")
(defvar dim-fgcolor "#6C7086"
  "The calm foreground of emacs.")
(defvar grim-fgcolor "#232338"
  "The calm foreground of emacs.")

;; Bars/ Lines blend
(set-face-attribute 'header-line nil :background bgcolor)
(set-face-attribute 'mode-line nil :background bgcolor)

;; Error Diagnostics
(setq flycheck-border-width 20)

;; Margin with automargin
(require 'automargin)
(automargin-mode)

;; modeline replacement
(require 'echo-bar)
(setq echo-bar-minibuffer nil)
(setq message-log-max nil)
(echo-bar-mode)

;; Disable "Beginning of Buffer" or "End of Buffer"
(defun my-command-error-function (data context caller)
  "Ignore the buffer-read-only, beginning-of-buffer,
end-of-buffer signals; pass the rest to the default handler."
  (when (not (memq (car data) '(buffer-read-only
                                beginning-of-buffer
                                end-of-buffer)))
    (command-error-default-function data context caller)))

(setq command-error-function #'my-command-error-function)

(defun suppress-message-advice-around (fun &rest args)
  (let (message-log-max)
    (with-temp-message (or (current-message) "")
      (apply fun args))))

;; example: suppress any messages from `save-buffer'
(advice-add 'save-buffer :around 'suppress-message-advice-around)
(advice-add 'kill-buffer :around 'suppress-message-advice-around)

(require 'base)
(require 'echo-bar-custom)
(require 'eldoc-box)

;; Custom Windows with custom vehaviours
(add-to-list 'display-buffer-alist '("\\*helpful.*"
                                     (display-buffer-in-side-window)
                                     (side . right)
                                     (window-width . 50)
                                     ))
(add-to-list 'display-buffer-alist '("\\.*embark.*"
                                     (display-buffer-in-side-window)
                                     (side . top)
                                     (window-width . 30)
                                     ))
(add-to-list 'display-buffer-alist '("\\.*lsp-ref.*"
                                     (display-buffer-in-side-window)
                                     (side . top)
                                     (window-width . 30)
                                     ))

;; FACES


(defun configure-face (frame)
  "Configure face given initial non-daemon FRAME.
Intended for `after-make-frame-functions'."
  (set-face-attribute 'eaBuf nil :weight 'bold :inherit 'ansi-color-white)
  (set-face-attribute 'eaDate nil :weight 'bold :inherit 'ansi-color-white)
  (set-face-attribute 'eaTime nil :weight 'bold :inherit 'ansi-color-white)
  (set-face-attribute 'eaBattery nil :weight 'bold :inherit 'ansi-color-white)

  (require 'prompt-text)
  (prompt-text-mode)

  (set-face-attribute 'vertico-posframe nil :weight 'bold :inherit 'vertico-multiline)
  (set-face-attribute 'vertico-posframe-border nil :inherit 'ansi-color-white)
  (set-face-attribute 'vertico-posframe-border-2 nil :inherit 'ansi-color-white)
  (set-face-attribute 'vertico-posframe-border-3 nil :inherit 'ansi-color-white)
  (set-face-attribute 'vertico-posframe-border-4 nil :inherit 'ansi-color-white)
  (set-face-attribute 'vertico-posframe-border-fallback nil :background darker-bgcolor :inherit 'ansi-color-white)
  (set-face-attribute 'vertico-current nil :foreground 'unspecified :weight 'bold :inherit 'org-footnote :background bgcolor)
  (set-face-attribute 'minibuffer-prompt nil :foreground 'unspecified :inherit 'org-document-title :height 220 :weight 'bold :family "Iosevka Nerd Font ExtraBold")

  (set-face-attribute 'flycheck-error nil :background "#42232c" :foreground "#F38BA8" :underline 'nil :weight 'bold)
  (set-face-attribute 'flycheck-info nil :background "#262d25" :foreground "#A6E3A1" :underline 'nil :weight 'bold)
  (set-face-attribute 'flycheck-warning nil :background "#453e29" :foreground "#F8D782" :underline 'nil :weight 'bold)
  
  (set-face-attribute 'diff-added nil :background "#262d25" :foreground "#F38BA8" :underline 'nil :weight 'bold)
  (set-face-attribute 'diff-removed nil :background "#42232c" :foreground "#A6E3A1" :underline 'nil :weight 'bold)
  
  (set-face-attribute 'flycheck-fringe-error nil :background 'unspecified :foreground bgcolor :underline 'nil)
  (set-face-attribute 'flycheck-fringe-info nil :background 'unspecified :foreground bgcolor :underline 'nil)
  (set-face-attribute 'flycheck-fringe-warning nil :background 'unspecified :foreground bgcolor :underline 'nil)
  (set-face-attribute 'flycheck-posframe-error-face nil :inherit 'flycheck-error :background darker-bgcolor :weight 'bold)
  (set-face-attribute 'flycheck-posframe-warning-face nil :inherit 'flycheck-warning :background darker-bgcolor :weight 'bold)
  (set-face-attribute 'flycheck-posframe-info-face nil :inherit 'flycheck-info :background darker-bgcolor :weight 'bold)
  (set-face-attribute 'flycheck-posframe-border-face nil :foreground darker-bgcolor :background darker-bgcolor)
  (set-face-attribute 'eldoc-box-body nil :background darker-bgcolor :weight 'bold)
  (set-face-attribute 'eldoc-box-border nil :background darker-bgcolor :weight 'bold)
  (set-face-attribute 'mono-complete-preview-face nil :foreground dim-fgcolor :background bgcolor)
  (set-face-attribute 'corfu-default nil :background darker-bgcolor :foreground dim-fgcolor :weight 'bold)
  (set-face-attribute 'corfu-current nil :foreground calm-fgcolor :background bgcolor :weight 'bold)

  (set-face-attribute 'pulsar-generic nil :background grim-fgcolor :weight 'bold)
  (with-current-buffer " *Echo Area 0*" (face-remap-add-relative 'default '(:family "Iosevka Nerd Font ExtraBold")))

  ;; ICONS
  (setq flycheck-posframe-warning-prefix "  ")
  (setq flycheck-posframe-error-prefix "󰚌  ")
  (setq flycheck-posframe-info-prefix "  ")
  (setq flycheck-posframe-border-width 20)

  (setq kind-icon-default-face 'corfu-default) ; to compute blended backgrounds correctly
  (setq kind-icon-use-icons 'nil)
  (setq kind-icon-blend-frac 0.08)
  (setq kind-icon-blend-background nil)
  (setq kind-icon-extra-space 'nil)
  (setq kind-icon-default-style '(:padding 1 :stroke 0 :margin 0 :radius 0 :height 1.0 :scale 1.0))
  (setq corfu--frame-parameters '((no-accept-focus . t)
                                  (no-focus-on-map . t)
                                  (min-width . t)
                                  (min-height . t)
                                  (border-width . 0)
                                  (child-frame-border-width . 10)
                                  (left-fringe . 0)
                                  (right-fringe . 0)
                                  (vertical-scroll-bars)
                                  (horizontal-scroll-bars)
                                  (menu-bar-lines . 0)
                                  (tool-bar-lines . 0)
                                  (tab-bar-lines . 0)
                                  (no-other-frame . t)
                                  (unsplittable . t)
                                  (undecorated . t)
                                  (cursor-type)
                                  (no-special-glyphs . t)
                                  (desktop-dont-save . t)))



  (remove-hook 'after-make-frame-functions #'configure-face))

(add-hook 'after-make-frame-functions #'configure-face)

(setq-default inhibit-message t)

(provide 'uiChanges)
;;; completed
