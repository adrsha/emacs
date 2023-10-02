(major-mode-hydra-define emacs-lisp-mode nil
  ("Eval"
   (("b" eval-buffer "buffer")
    ("e" eval-defun "defun")
    ("r" eval-region "region"))
   "Test"
   (("t" ert "prompt")
    ("T" (ert t) "all")
    ("F" (ert :failed) "failed"))
   "Doc"
   (("d" describe-foo-at-point "thing-at-pt")
    ("f" describe-function "function")
    ("v" describe-variable "variable")
    ("i" info-lookup-symbol "info lookup"))))

(pretty-hydra-define jp-window (:foreign-keys warn :title jp-window--title :quit-key "q")
   "Resize"
   (("h" move-border-left "←")
    ("j" move-border-down "↓")
    ("k" move-border-up "↑")
    ("l" move-border-right "→")
    ("n" balance-windows "balance")
    ("f" toggle-frame-fullscreen "toggle fullscreen"))

   "Split"
   (("b" split-window-right "horizontally")
    ("B" split-window-horizontally-instead "horizontally instead")
    ("v" split-window-below "vertically")
    ("V" split-window-vertically-instead "vertically instead"))

   "Zoom"
   (("+" zoom-in "in")
    ("=" zoom-in)
    ("-" zoom-out "out")
    ("0" jp-zoom-default "reset"))))

;; (use-package helpful
;;   :ensure t
;;   :pretty-hydra
;;   ((:color teal :quit-key "q")
;;    ("Helpful"
;;     (("f" helpful-callable "callable")
;;      ("v" helpful-variable "variable")
;;      ("k" helpful-key "key")
;;      ("c" helpful-command "command")
;;      ("d" helpful-at-point "thing at point"))))
;;
;;   :bind ("C-h" . helpful-hydra/body))

(provide 'hydra-setup)
