;; Requires projectile to be installed

(set-face-attribute 'header-line nil :background "#11111B")
(set-face-attribute 'mode-line nil :background "#11111B")

(require 'tabspaces)

;; Margin with writemode
(require 'automargin)
(automargin-mode)

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

(require 'base)
(require 'echo-bar-custom)


(add-to-list 'display-buffer-alist '("\\*helpful.*"
                                     (display-buffer-in-side-window)
                                     (side . right)
                                     (window-width . 50)
                                     ))

(with-current-buffer " *Echo Area 0*" (face-remap-add-relative 'default '(:family "Iosevka Nerd Font ExtraBold")))

(set-face-attribute 'eaBuf nil :weight 'bold :foreground "#b5bfe8")
(set-face-attribute 'eaDate nil :weight 'bold :foreground "#b5bfe8")
(set-face-attribute 'eaTime nil :weight 'bold :foreground "#b5bfe8")
(set-face-attribute 'eaBattery nil :weight 'bold :foreground "#b5bfe8")



(require 'eldoc-box)

(set-face-attribute 'eldoc-box-body nil :weight 'bold :foreground "#b5bfe8" :background "#0B0B11")
(set-face-attribute 'eldoc-box-border nil :weight 'bold :foreground "#b5bfe8" :background "#0B0B11")


(setq-default inhibit-message t)
(provide 'uiChanges)
