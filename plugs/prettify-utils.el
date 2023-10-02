(font-lock-add-keywords 'org-mode
   '(("^ *\\([-]\\) "
   (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "✧"))))))
  (font-lock-add-keywords 'org-mode
   '(("^ *\\([+]\\) "
   (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "✦"))))))

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
          ("CLOSED:"        . ?⊡) ; I abuse some math notation.
          ("SCHEDULED:"     . ?⊞) ; It's a window - not a plus sign in a box
          ("DEADLINE:"      . ?⊠) ; and now the window has been blocked.
          (":PROPERTIES:"   . ?⚙)
          (":LOGBOOK:"      . ?☰) ; Same width as the gear in Ubuntu mono.
          ("CLOCK:"         . ?⧖) ; Other items in the logbook have a bullet.
          ("[-]"            . ?⊟) ; different from the other ballot icons.
          ("[#A]"           . ?🄰)
          ("[#B]"           . ?🄱)
          ("[#C]"           . ?🄲)
          ("TODO" . "☐")
          ("DONE" . "☑")
          ("|" . "│")
          ("[ ]" . "☐")
          ("[X]" . "☑")
          ("[-]" . "❍")
          ))
  (prettify-symbols-mode 1))

(add-hook 'org-mode-hook 'my/org-mode/load-prettify-symbols)


;; PRETTIFYING ORG
(setq org-ellipsis " ❋")
(setq org-bullets-bullet-list '("⇾ " "⇾ " "⇾ " "⇾ " "⇾ " "⇾ "))
;; (setq org-bullets-bullet-list '(" " " " " " " " " " " "))
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))



(defun prettify-set ()
  (interactive)
  (setq prettify-symbols-alist
        '(("lambda" .  "λ")))
  (prettify-symbols-mode 1))
(add-hook 'prog-mode-hook 'prettify-set)
(add-hook 'prog-mode-hook 'rainbow-mode)
(add-hook 'org-mode-hook 'rainbow-mode)

  ;; Enable all Iosevka ligatures in programming modes
(ligature-set-ligatures 'prog-mode '("<---" "<--"  "<<-" "<-" "->" "-->" "--->" "<->" "<-->" "<--->" "<---->" "<!--" "<==" "<===" "<=" "=>" "=>>" "==>" "===>" ">=" "<=>" "<==>" "<===>" "<====>" "<!---" "<~~" "<~" "~>" "~~>" "::" ":::" "==" "!=" "===" "!==" ":=" ":-" ":+" "<*" "<*>" "*>" "<|" "<|>" "|>" "+:" "-:" "=:" "<******>" "++" "+++"))
;; Enables ligature checks globally in all buffers. You can also do it
;; per mode with `ligature-mode'.
(global-ligature-mode t)

(use-package hl-todo
             :hook ((org-mode . hl-todo-mode)
                    (prog-mode . hl-todo-mode))
             :config
             (setq hl-todo-highlight-punctuation ":"
                   hl-todo-keyword-faces
                   `(("TODO"       outline-1 bold)
                     ("FIXME"      error bold)
                     ("ERROR"      error bold)
                     ("INFO"       outline-1 bold)
                     ("SUCCESS"    success bold)
                     ("DONE"    success bold)
                     ("HACK"       font-lock-constant-face bold)
                     ("WARN"       warning bold)
                     ("REVIEW"     font-lock-keyword-face bold)
                     ("NOTE"       success bold)
                     ("DEPRECATED" shadow bold))))


(provide 'prettify-utils)
