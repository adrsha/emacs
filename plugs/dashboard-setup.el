(add-hook 'elpaca-after-init-hook #'dashboard-insert-startupify-lists)
(add-hook 'elpaca-after-init-hook #'dashboard-initialize)
(dashboard-setup-startup-hook)

;; Set the title
(setq dashboard-banner-logo-title "Make shit happen.") 
;; Set the banner
(setq dashboard-startup-banner "~/.config/emacs/images/emacs-logo.png")
;; Value can be
;; - nil to display no banner
;; - 'official which displays the official emacs logo
;; - 'logo which displays an alternative emacs logo
;; - 1, 2 or 3 which displays one of the text banners
;; - "path/to/your/image.gif", "path/to/your/image.png" or "path/to/your/text.txt" which displays whatever gif/image/text you would prefer
;; - a cons of '("path/to/your/image.png" . "path/to/your/text.txt")

;; Content is not centered by default. To center, set
(setq dashboard-center-content t)
(setq dashboard-set-footer nil)
(setq dashboard-set-init-info 'nil)

(dolist (mode '(dashboard-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(setq dashboard-show-shortcuts nil)
(setq dashboard-items 'nil)
(evil-collection-dashboard-setup)

(provide 'dashboard-setup)
