  ;; make sure which-key doesn't show normally but refreshes quickly after it is triggered.
(setq which-key-idle-delay 0.5)
(setq which-key-idle-secondary-delay 0.05)
(setq which-key-popup-type 'side-window)
(which-key-mode)


(set-face-attribute 'which-key-group-description-face nil :foreground "#CBA6F7" :weight 'bold)
(set-face-attribute 'which-key-command-description-face nil :foreground "#B2BFE8" :weight 'bold)
(set-face-attribute 'which-key-special-key-face nil :foreground "#B2BFE8" :background nil :weight 'bold)
(set-face-attribute 'which-key-key-face nil :foreground "#89B4FA" :weight 'bold)
(provide 'whichkey-setup)
