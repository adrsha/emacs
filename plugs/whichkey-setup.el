(use-package which-key
  :config
  ;; make sure which-key doesn't show normally but refreshes quickly after it is triggered.
  (setq which-key-idle-delay 0.5)
  (setq which-key-idle-secondary-delay 0.05)
  (setq which-key-popup-type 'side-window)
  (which-key-mode))

(provide 'whichkey-setup)
