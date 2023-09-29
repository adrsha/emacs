
(set-face-attribute 'default nil :font "Iosevka Nerd Font Medium" :height 150)
(set-face-attribute 'fixed-pitch nil :font "Iosevka Nerd Font Medium" :height 150)
(set-face-attribute 'font-lock-comment-face nil :slant 'italic)
(set-face-attribute 'font-lock-keyword-face nil :slant 'italic)
(set-face-attribute 'line-number nil :font "JetBrainsMono Nerd Font Bold" :height 120)
(add-to-list 'default-frame-alist '(font . "Iosevka Nerd Font"))


(defun my-configure-font (frame)
  "Configure font given initial non-daemon FRAME.
Intended for `after-make-frame-functions'."
  ;; Do stuff with FRAME...
(set-face-attribute 'default nil :font "Iosevka Nerd Font Medium" :height 150)
(set-face-attribute 'fixed-pitch nil :font "Iosevka Nerd Font Medium" :height 150)
(set-face-attribute 'font-lock-comment-face nil :slant 'italic)
(set-face-attribute 'font-lock-keyword-face nil :slant 'italic)
(set-face-attribute 'line-number nil :font "JetBrainsMono Nerd Font Bold" :height 120)
(add-to-list 'default-frame-alist '(font . "Iosevka Nerd Font"))

  (remove-hook 'after-make-frame-functions #'my-configure-font))

(add-hook 'after-make-frame-functions #'my-configure-font)

(provide 'fonts)