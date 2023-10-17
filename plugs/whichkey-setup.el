;; ░██╗░░░░░░░██╗██╗░░██╗██╗░█████╗░██╗░░██╗██╗░░██╗███████╗██╗░░░██╗
;; ░██║░░██╗░░██║██║░░██║██║██╔══██╗██║░░██║██║░██╔╝██╔════╝╚██╗░██╔╝
;; ░╚██╗████╗██╔╝███████║██║██║░░╚═╝███████║█████═╝░█████╗░░░╚████╔╝░
;; ░░████╔═████║░██╔══██║██║██║░░██╗██╔══██║██╔═██╗░██╔══╝░░░░╚██╔╝░░
;; ░░╚██╔╝░╚██╔╝░██║░░██║██║╚█████╔╝██║░░██║██║░╚██╗███████╗░░░██║░░░
;; ░░░╚═╝░░░╚═╝░░╚═╝░░╚═╝╚═╝░╚════╝░╚═╝░░╚═╝╚═╝░░╚═╝╚══════╝░░░╚═╝░░░

(require 'which-key-posframe)
;; Enable
(which-key-mode)
(which-key-posframe-mode)

;; Delays
(setq which-key-idle-delay 0.3)
(setq which-key-idle-secondary-delay 0.02)

;; Display
;; (setq which-key-popup-type 'frame)
;; (setq which-key-popup-type 'side-window)
;; (setq which-key-side-window-location 'bottom)
;; (setq which-key-prefix-prefix "󰜡 ")
(setq which-key-frame-max-width 100)

(setq which-key-sort-order 'which-key-prefix-then-key-order)
(setq which-key-prefix-prefix "")
;; (setq which-key-max-display-columns 0)
(setq which-key-add-column-padding 0)

;; Replacements
;; (add-to-list 'which-key-replacement-alist '(("TAB" . nil) . ("↹" . nil)))
;; (add-to-list 'which-key-replacement-alist '(("RET" . nil) . ("⏎" . nil)))
;; (add-to-list 'which-key-replacement-alist '(("DEL" . nil) . ("⇤" . nil)))
;; (add-to-list 'which-key-replacement-alist '(("SPC" . nil) . ("󱁐" . nil)))

;; Faces
(set-face-attribute 'which-key-group-description-face nil :weight 'bold)
(set-face-attribute 'which-key-command-description-face nil :weight 'bold)
(set-face-attribute 'which-key-key-face nil :weight 'bold :inherit 'org-drawer)

;; whichkey posframe
(setq which-key-posframe-poshandler 'posframe-poshandler-frame-bottom-center)

(setq which-key-posframe-border-width 20)

(provide 'whichkey-setup)
