;; (use-package base16-theme
;;   :ensure t
;;   :config
;;   (load-theme 'base16-default-dark t))
(use-package catppuccin-theme)
(load-theme 'catppuccin :no-confirm)
(catppuccin-set-color 'base "#11111B") ;; change base to #000000 for the currently active flavor
(catppuccin-set-color 'crust "#0B0B11") ;; change crust to #222222 for frappe
(catppuccin-reload)
(provide 'colorscheme)
