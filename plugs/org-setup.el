
;; ░█████╗░██████╗░░██████╗░░░░░░░███╗░░░███╗░█████╗░██████╗░███████╗
;; ██╔══██╗██╔══██╗██╔════╝░░░░░░░████╗░████║██╔══██╗██╔══██╗██╔════╝
;; ██║░░██║██████╔╝██║░░██╗░█████╗██╔████╔██║██║░░██║██║░░██║█████╗░░
;; ██║░░██║██╔══██╗██║░░╚██╗╚════╝██║╚██╔╝██║██║░░██║██║░░██║██╔══╝░░
;; ╚█████╔╝██║░░██║╚██████╔╝░░░░░░██║░╚═╝░██║╚█████╔╝██████╔╝███████╗
;; ░╚════╝░╚═╝░░╚═╝░╚═════╝░░░░░░░╚═╝░░░░░╚═╝░╚════╝░╚═════╝░╚══════╝



;; ShortCuts
(require 'org-tempo)
(add-to-list 'org-structure-template-alist '("sh" . "src shell"))
(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
(add-to-list 'org-structure-template-alist '("py" . "src python"))

;; FONTS
(defun my-configure-font (frame)
  "Configure font given initial non-daemon FRAME.
Intended for `after-make-frame-functions'."

  ;; Do stuff with FRAME...
  (with-eval-after-load 'org-faces
    (set-face-attribute 'org-block nil :background "#0B0B11")
    (set-face-attribute 'org-block-end-line nil :background "#0B0B11")
    (set-face-attribute 'org-block-begin-line nil :background "#0B0B11")
    ;; Set faces for heading levels
    (dolist (face '((org-meta-line . 0.7)
                    (org-level-1 . 1.7)
                    (org-level-2 . 1.5)
                    (org-level-3 . 1.4)
                    (org-level-4 . 1.3)
                    (org-level-5 . 1.3)
                    (org-level-6 . 1.3)
                    (org-level-7 . 1.2)
                    (org-level-8 . 1.1)))
      (set-face-attribute (car face) nil :font "Barlow" :height (cdr face))))

  (remove-hook 'after-make-frame-functions #'my-configure-font))

(add-hook 'after-make-frame-functions #'my-configure-font)

;; Hide org markup
(setq-default org-hide-emphasis-markers t)

;; Indentation
(setq org-startup-folded nil)
(org-indent-mode 1)
(setq org-level-color-stars-only nil)
(setq org-hide-leading-stars nil)

;;Languages

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (python . t)))

(push '("conf-unix" . conf-unix) org-src-lang-modes)
(setq org-confirm-babel-evaluate nil)


;; Custom pairs for electric pair
(defvar org-electric-pairs '((?/ . ?/) (?= . ?=)) "Electric pairs for org-mode.")

(defun org-add-electric-pairs ()
  (setq-local electric-pair-pairs (append electric-pair-pairs org-electric-pairs))
  (setq-local electric-pair-text-pairs electric-pair-pairs))

;; Disable the autocompletion of pairs <>
(add-hook 'org-mode-hook (lambda () (setq-local electric-pair-inhibit-predicate `(lambda (c) (if (char-equal c ?<) t (,electric-pair-inhibit-predicate c))))))


(provide 'org-setup)
