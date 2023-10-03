;; ░██╗░░░░░░░██╗██╗░░██╗██╗░█████╗░██╗░░██╗██╗░░██╗███████╗██╗░░░██╗
;; ░██║░░██╗░░██║██║░░██║██║██╔══██╗██║░░██║██║░██╔╝██╔════╝╚██╗░██╔╝
;; ░╚██╗████╗██╔╝███████║██║██║░░╚═╝███████║█████═╝░█████╗░░░╚████╔╝░
;; ░░████╔═████║░██╔══██║██║██║░░██╗██╔══██║██╔═██╗░██╔══╝░░░░╚██╔╝░░
;; ░░╚██╔╝░╚██╔╝░██║░░██║██║╚█████╔╝██║░░██║██║░╚██╗███████╗░░░██║░░░
;; ░░░╚═╝░░░╚═╝░░╚═╝░░╚═╝╚═╝░╚════╝░╚═╝░░╚═╝╚═╝░░╚═╝╚══════╝░░░╚═╝░░░

;; Enable
(which-key-mode)

;; Delays
(setq which-key-idle-delay 0.3)
(setq which-key-idle-secondary-delay 0.02)

;; Display
;; (setq which-key-popup-type 'frame)
(setq which-key-popup-type 'side-window)
(setq which-key-side-window-location 'bottom)
(setq which-key-side-window-max-width 0.33)
(setq which-key-side-window-max-height 0.25)
;; (setq which-key-prefix-prefix "󰜡 ")
(setq which-key-prefix-prefix "")
(setq which-key-max-display-columns 4)
(setq which-key-sort-order 'which-key-prefix-then-key-order)
(setq which-key-add-column-padding 3)

;; Replacements
(add-to-list 'which-key-replacement-alist '(("TAB" . nil) . ("↹" . nil)))
(add-to-list 'which-key-replacement-alist '(("RET" . nil) . ("⏎" . nil)))
(add-to-list 'which-key-replacement-alist '(("DEL" . nil) . ("⇤" . nil)))
(add-to-list 'which-key-replacement-alist '(("SPC" . nil) . ("󱁐" . nil)))

;; Faces
(set-face-attribute 'which-key-group-description-face nil :foreground "#CBA6F7" :weight 'bold)
(set-face-attribute 'which-key-command-description-face nil :foreground "#6C7086" :weight 'bold)
(set-face-attribute 'which-key-key-face nil :foreground "#89B4FA" :weight 'bold)
(provide 'whichkey-setup)
