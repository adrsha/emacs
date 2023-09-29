
(defun my/org-mode/load-prettify-symbols ()
  (interactive)
  (setq prettify-symbols-alist
        '(("#+begin_src" . " ")
          ("#+BEGIN_SRC" . " ")
          ("#+end_src" . " ")
          ("#+END_SRC" . " ")
          ("#+title:" . " ")
          ("#+TITLE:" . " ")
          ("#+begin_example" . ?)
          ("#+BEGIN_EXAMPLE" . ?)
          ("#+end_example" . ?)
          ("#+END_EXAMPLE" . ?)
          ("#+header:" . ?)
          ("#+HEADER:" . ?)
          ("#+name:" . ?﮸)
          ("#+NAME:" . ?﮸)
          ("#+results:" . ?)
          ("#+RESULTS:" . ?)
          ("#+call:" . ?)
          ("#+CALL:" . ?)
          (":PROPERTIES:" . ?)
          (":properties:" . ?)
          ("TODO" . "☐")
          ("DONE" . "☑")
          ("|" . "│")
          ("[ ]" . "☐")
          ("[X]" . "☑")
          ("[-]" . "❍")
          ))
  (prettify-symbols-mode 1))

(add-hook 'org-mode-hook 'my/org-mode/load-prettify-symbols)

(defun prettify-set ()
  (interactive)
  (setq prettify-symbols-alist
        '(("lambda" .  "λ")))
  (prettify-symbols-mode 1))
(add-hook 'prog-mode-hook 'prettify-set)

(use-package ligature
  :config
  ;; Enable all Iosevka ligatures in programming modes
  (ligature-set-ligatures 'prog-mode '("<---" "<--"  "<<-" "<-" "->" "-->" "--->" "<->" "<-->" "<--->" "<---->" "<!--" "<==" "<===" "<=" "=>" "=>>" "==>" "===>" ">=" "<=>" "<==>" "<===>" "<====>" "<!---" "<~~" "<~" "~>" "~~>" "::" ":::" "==" "!=" "===" "!==" ":=" ":-" ":+" "<*" "<*>" "*>" "<|" "<|>" "|>" "+:" "-:" "=:" "<******>" "++" "+++"))
  ;; Enables ligature checks globally in all buffers. You can also do it
  ;; per mode with `ligature-mode'.
  (global-ligature-mode t))


(use-package hl-todo
  :hook ((org-mode . hl-todo-mode)
         (prog-mode . hl-todo-mode))
  :config
  (setq hl-todo-highlight-punctuation ":"
        hl-todo-keyword-faces
        `(("TODO"       warning bold)
          ("FIXME"      error bold)
          ("ERROR"      error bold)
          ("SUCCESS"    success bold)
          ("HACK"       font-lock-constant-face bold)
          ("WARN"       warning bold)
          ("REVIEW"     font-lock-keyword-face bold)
          ("NOTE"       success bold)
          ("DEPRECATED" shadow bold))))


(provide 'prettify-utils)
