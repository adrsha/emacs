;; (use-package base16-theme
;;   :ensure t
;;   :config
;;   (load-theme 'base16-default-dark t))
(load-theme 'catppuccin :no-confirm)

(catppuccin-set-color 'base "#11111B") 
(catppuccin-set-color 'crust "#0B0B11")
(catppuccin-reload)
(provide 'colorscheme)
