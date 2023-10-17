(add-hook 'elpaca-after-init-hook 'hydra-posframe-mode )
(setq hydra-posframe-border-width 40)
(setq hydra-posframe-poshandler 'posframe-poshandler-frame-bottom-right-corner)

(defhydra hydra-hydras (:color teal)
  "
_t_: Toggles   _o_: Org

"
  ("t" hydra-toggle/body nil)
  ("o" hydra-org/body nil)
  ("q" nil "Quit")
  )

;; |----------+-----------+-----------------------+-----------------|
;; | Body     | Head      | Executing NON-HEADS   | Executing HEADS |
;; | Color    | Inherited |                       |                 |
;; |          | Color     |                       |                 |
;; |----------+-----------+-----------------------+-----------------|
;; | amaranth | red       | Disallow and Continue | Continue        |
;; | teal     | blue      | Disallow and Continue | Quit            |
;; | pink     | red       | Allow and Continue    | Continue        |
;; | red      | red       | Allow and Quit        | Continue        |
;; | blue     | blue      | Allow and Quit        | Quit            |
;; |----------+-----------+-----------------------+-----------------|

(defhydra hydra-org (:color pink)
  "
_b_: Org-bullets-mode: %`org-bullets-mode
^
^_l_: Org-Toggle link display
^_c_: Org-cycle
^_s_: Insert Schedule
"
  ("s" org-schedule nil)
  ("l" org-toggle-link-display nil)
  ("b" org-bullets-mode nil)
  ("c" org-cycle-global nil)
  ("q" nil "Quit"))

(defhydra hydra-toggle (:color pink)
  "
_a_ abbrev-mode:       %`abbrev-mode
_d_ debug-on-error:    %`debug-on-error
_f_ auto-fill-mode:    %`auto-fill-function
_t_ truncate-lines:    %`truncate-lines
_n_ line-numbers:      %`global-display-line-numbers-mode

"
  ("a" abbrev-mode nil)
  ("d" toggle-debug-on-error nil)
  ("f" auto-fill-mode nil)
  ("t" toggle-truncate-lines nil)
  ("n" global-display-line-numbers-mode nil)
  ("q" nil "quit"))

(defhydra hydra-insert-cursor (:color pink)
  "
_n_: Select and Move next Match ^_p_: Select and Move previous match
^_N_: Skip and goto next match  ^_P_: Skip and goto previous match
^_C-n_: Make cursor next line  ^_C-p_: Make cursor previous line
"
  ("n" evil-mc-make-and-goto-next-match nil)
  ("p" evil-mc-make-and-goto-prev-match nil)
  ("N" evil-mc-skip-and-goto-next-match nil)
  ("P" evil-mc-skip-and-goto-prev-match nil)
  ("C-n" evil-mc-make-cursor-move-next-line nil )
  ("C-p" evil-mc-make-cursor-move-prev-line nil )
  ("q" nil "Quit")
  ("Esc" nil nil))



(provide 'hydras)
