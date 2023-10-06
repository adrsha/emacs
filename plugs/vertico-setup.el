;; ██╗░░░██╗███████╗██████╗░████████╗██╗░█████╗░░█████╗░
;; ██║░░░██║██╔════╝██╔══██╗╚══██╔══╝██║██╔══██╗██╔══██╗
;; ╚██╗░██╔╝█████╗░░██████╔╝░░░██║░░░██║██║░░╚═╝██║░░██║
;; ░╚████╔╝░██╔══╝░░██╔══██╗░░░██║░░░██║██║░░██╗██║░░██║
;; ░░╚██╔╝░░███████╗██║░░██║░░░██║░░░██║╚█████╔╝╚█████╔╝
;; ░░░╚═╝░░░╚══════╝╚═╝░░╚═╝░░░╚═╝░░░╚═╝░╚════╝░░╚════╝░
;;
;;

;; Enable
(vertico-mode)

;; Nerd Icons
(add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup)

;; Enable saving history for all things completion
(savehist-mode)

;; show the lower options a little earlier
(setq vertico-scroll-margin 2)
(setq vertico-cycle t)

;; Completion styles
(setq completion-styles '(orderless basic substring partial-completion ))
(setq completion-category-overrides '((file (styles basic partial-completion))))

;; Previewing the selected element
(add-hook 'completion-list-mode #'consult-preview-at-point-mode)

(setq register-preview-delay 0.5 register-preview-function #'consult-register-format)
(advice-add #'register-preview :override #'consult-register-window)

(setq xref-show-xrefs-function #'consult-xref
      xref-show-definitions-function #'consult-xref)

;; Allowing single key press to begin asynchorous searches like consult-grep
(setq consult-async-min-input 1)

;; Preview the current element
(setq consult-preview-key "C-h")

;; Buffers to ignore for consult
(add-to-list 'consult-buffer-filter "\*.*\*")

(consult-customize
 consult-ripgrep consult-git-grep consult-grep
 consult-bookmark consult-xref
 consult--source-bookmark consult--source-file-register
 consult--source-recent-file
 :preview-key '(:debounce 0.1 any)) ;; Option 1: Automatic preview

(consult-customize
 consult-buffer
 :preview-key '(:debounce 0.1 any)) ;; Option 1: Automatic preview
;; :preview-key "C-h")            ;; Option 2: Manual preview

;; Documentation in the menu itself
(marginalia-mode)

;; Enable embark help
(setq prefix-help-command #'embark-prefix-help-command)

(provide 'vertico-setup)
