(use-package autoinsert
  :config
  (setq auto-insert-query nil)             ; disable the default auto-inserts
  (auto-insert-mode 1)                     ; enable auto-insert-mode globally
  (add-hook 'find-file-hook 'auto-insert)  ; insert templates when we C-x C-f new files
  (setq auto-insert-alist nil)             ; remove this line to restore defaults

  ;; add our "competitive coding" templates

  ;; a basic example
  (add-to-list 'auto-insert-alist
               (cons ".+\\.cpp\\'" 
                     (expand-file-name "~/.config/emacs/plugs/templates/cpp.txt")))

  (add-to-list 'auto-insert-alist
               (cons ".+\\.org\\'" 
                     (expand-file-name "~/.config/emacs/plugs/templates/org.txt")))

  ;; (add-to-list 'auto-insert-alist
  ;;              (cons (concat "^" (expand-file-name "~/competitiveCoding/") ".+\\.hh\\'")
  ;;                    (expand-file-name "~/competitiveCoding/hh_template.hh")))

  )
