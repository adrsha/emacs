
;; ░█████╗░██████╗░░██████╗░░░░░░░███╗░░░███╗░█████╗░██████╗░███████╗
;; ██╔══██╗██╔══██╗██╔════╝░░░░░░░████╗░████║██╔══██╗██╔══██╗██╔════╝
;; ██║░░██║██████╔╝██║░░██╗░█████╗██╔████╔██║██║░░██║██║░░██║█████╗░░
;; ██║░░██║██╔══██╗██║░░╚██╗╚════╝██║╚██╔╝██║██║░░██║██║░░██║██╔══╝░░
;; ╚█████╔╝██║░░██║╚██████╔╝░░░░░░██║░╚═╝░██║╚█████╔╝██████╔╝███████╗
;; ░╚════╝░╚═╝░░╚═╝░╚═════╝░░░░░░░╚═╝░░░░░╚═╝░╚════╝░╚═════╝░╚══════╝


(add-to-list 'load-path "~/.config/emacs/plugs/org-fc/")

(require 'org-eldoc)
(require 'org-tempo)
(require 'org-fc)
(require 'org-fc-hydra)

;; ShortCuts
(add-to-list 'org-structure-template-alist '("sh" . "src shell"))
(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
(add-to-list 'org-structure-template-alist '("py" . "src python :results output "))
(add-to-list 'org-structure-template-alist '("cpp" . "src C++ :results verbatim \n\n  #include <iostream>\n  using namespace std;\n\n  int main(){\n    return 0;\n}"))
;; Customization

;; Save Org buffers after refiling!
(advice-add 'org-refile :after 'org-save-all-org-buffers)
(org-eldoc-load)

(setq org-tag-alist
      '((:startgroup)
		(:endgroup)
		("home" . ?H)
		("work" . ?W)
		("agenda" . ?a)
		("planning" . ?p)
		("publish" . ?P)
		("batch" . ?b)
		("note" . ?n)
		("idea" . ?i)))

(defun org-config (frame)
  "Configure Org mode things. Intended for `after-make-frame-functions'."
  (setq org-agenda-start-with-log-mode t)
  (setq org-log-done 'time)
  (setq org-agenda-span 10)
  (setq org-agenda-start-on-weekday nil)
  (setq org-log-into-drawer t)

  (set-face-attribute 'org-hide nil :inherit 'fixed-pitch)
  ;; (remove-hook 'after-make-frame-functions 'org-config)
  )
(add-hook 'after-make-frame-functions 'org-config)

;; Hide org markup
(setq-default org-hide-emphasis-markers t)

;; Indentation
(setq org-startup-folded t)
(org-indent-mode 1)
(setq org-edit-src-content-indentation 2)
(setq org-src-preserve-indentation nil)

(with-eval-after-load 'org
  (org-babel-do-load-languages
      'org-babel-load-languages
      '((C . t)
        (emacs-lisp . t)
      (python . t)))

  (push '("conf-unix" . conf-unix) org-src-lang-modes))



(setq org-src-fontify-natively t
      org-src-window-setup 'current-window
      org-src-strip-leading-and-trailing-blank-lines t
      org-src-preserve-indentation 'nil
      org-src-tab-acts-natively t)

;; Agenda

(setq org-agenda-files '("~/Documents/notes/home.org")
	  org-agenda-breadcrumbs-separator " ❱ "
	  org-directory "~/Documents/"
	  )
(defun my-org-agenda-format-date-aligned (date)
  "Format a DATE string for display in the daily/weekly agenda, or timeline.
This function makes sure that dates are aligned for easy reading."
  (require 'cal-iso)
  (let* ((dayname (calendar-day-name date nil nil))
         (day (cadr date))
         (day-of-week (calendar-day-of-week date))
         (month (car date))
         (monthname (calendar-month-name month 1))
         (year (nth 2 date))
         (iso-week (org-days-to-iso-week
                    (calendar-absolute-from-gregorian date)))
         (weekyear (cond ((and (= month 1) (>= iso-week 52))
                          (1- year))
                         ((and (= month 12) (<= iso-week 1))
                          (1+ year))
                         (t year)))
         (weekstring (if (= day-of-week 1)
                         (format " W%02d" iso-week)
                       "")))
	(format " %-2s %2d %s" dayname day monthname)
	))

(setq org-agenda-hidden-separator "‌‌ ")
(setq org-agenda-block-separator (string-to-char " "))
(setq org-agenda-format-date 'my-org-agenda-format-date-aligned)
(setq org-agenda-block-separator nil)

(defun agenda-color-char ()
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "" nil t)
      (put-text-property (match-beginning 0) (match-end 0)
                         'face '(:height 220 :foreground "gold2" :bold t)))))


(setq org-agenda-custom-commands
      '(
        ("a" "My Agenda"
         (
          (agenda "" (
                      (org-agenda-skip-scheduled-if-done nil)
                      (org-agenda-time-leading-zero t)
                      (org-agenda-timegrid-use-ampm nil)
                      (org-agenda-skip-timestamp-if-done t)
                      (org-agenda-skip-deadline-if-done t)
                      (org-agenda-start-day "+0d")
                      (org-agenda-span 5)
                      (org-agenda-overriding-header " Calendar")
                      (org-agenda-repeating-timestamp-show-all nil)
                      (org-agenda-remove-tags t)
                      ;; (org-agenda-prefix-format "%i %?-2 t%s")
                      (org-agenda-prefix-format "  %?-2 t%s")
                      ;; (concat "  %-3i  %-15b %t%s" org-agenda-hidden-separator)
                      (org-agenda-todo-keyword-format " ☐ ")
                      ;; (org-agenda-todo-keyword-format "")
                      (org-agenda-time)
                      (org-agenda-current-time-string "────────── Now ")
                      (org-agenda-scheduled-leaders '("" ""))
                      (org-agenda-deadline-leaders '("Deadline:  " "In %3d d.: " "%2d d. ago: "))
                      (org-agenda-time-grid (quote ((today require-timed remove-match) () "      " "─────────────")))))

          (tags "-CATEGORY=\"work\"+TODO=\"TODO\"|-CATEGORY=\"work\"+TODO=\"DONE\"" (
																					 (org-agenda-overriding-header "\n Today")
																					 (org-agenda-sorting-strategy '(priority-down))
																					 (org-agenda-remove-tags t)
																					 (org-agenda-skip-function '(org-agenda-skip-entry-if 'timestamp 'scheduled))
																					 ;; (org-agenda-todo-ignore-scheduled 'all)
																					 (org-agenda-prefix-format "   %-2i ")
																					 ;; (org-agenda-todo-keyword-format "")
																					 ))

          (tags "-CATEGORY=\"work\"+TODO=\"NEXT\"" (
													(org-agenda-overriding-header " Next")
													(org-agenda-sorting-strategy '(priority-down))
													(org-agenda-remove-tags t)
													;; (org-agenda-skip-function '(org-agenda-skip-entry-if 'timestamp))
													(org-agenda-todo-ignore-scheduled 'all)
													(org-agenda-prefix-format "   %-2i %?b")
													(org-agenda-todo-keyword-format "")))


          (tags "+project-CATEGORY=\"work\"" (
                                              (org-agenda-overriding-header " Projects")
                                              (org-agenda-remove-tags t)
                                              (org-tags-match-list-sublevels nil)
                                              (org-agenda-show-inherited-tags nil)
                                              (org-agenda-prefix-format "   %-2i %?b")
                                              (org-agenda-todo-keyword-format "")))
          ))

		))



;; Allow eglot in org-src modes
(defun e/org-babel-edit()
  "Edit python src block with lsp support by tangling the block and
then setting the org-edit-special buffer-file-name to the
absolute path. Finally load eglot."
  (interactive)

;; org-babel-get-src-block-info returns lang, code_src, and header
;; params; Use nth 2 to get the params and then retrieve the :tangle
;; to get the filename
  (setq mb/tangled-file-name (expand-file-name (assoc-default :tangle (nth 2 (org-babel-get-src-block-info)))))

  ;; tangle the src block at point 
  (org-babel-tangle '(4))
  (org-edit-special)

  ;; Now we should be in the special edit buffer with python-mode. Set
  ;; the buffer-file-name to the tangled file so that pylsp and
  ;; plugins can see an actual file.
  (setq-local buffer-file-name mb/tangled-file-name)
  (eglot-ensure)
  )

;;Languages
(push '("conf-unix" . conf-unix) org-src-lang-modes)
(setq org-confirm-babel-evaluate nil)




;; FIXME this doesnt seem to work for some reason
;; Custom pairs for electric pair
(defvar org-electric-pairs '((?/ . ?/) (?= . ?=)) "Electric pairs for org-mode.")

;; Disable the autocompletion of pairs <>
(add-hook 'org-mode-hook (lambda () (setq-local electric-pair-inhibit-predicate `(lambda (c) (if (char-equal c ?<) t (,electric-pair-inhibit-predicate c))))))
(defun org-add-electric-pairs ()
  (interactive)
  (setq-local electric-pair-pairs (append electric-pair-pairs org-electric-pairs))
  (setq-local electric-pair-text-pairs electric-pair-pairs))




(add-hook 'org-mode-hook (lambda () (variable-pitch-mode 1) (org-add-electric-pairs) (turn-on-visual-line-mode) ))

;; Configure custom agenda views
(setq org-capture-templates
      `(("t" "Tasks / Projects")
		("tt" "Task" entry (file+olp "~/Documents/notes/home.org" "Inbox")
         "* TODO %?\n  %i")
		("th" "Homework" entry (file+olp "~/Documents/notes/home.org" "Inbox")
         "* TODO %?\n  %i")
        ;; "* TODO %?\n  %U\n  %a\n  %i" :empty-lines 1)
		;; ("j" "Journal Entries")
		;; ("jj" "Journal" entry
		;;      (file+olp+datetree "~/Projects/Code/emacs-from-scratch/OrgFiles/Journal.org")
		;;      "\n* %<%I:%M %p> - Journal :journal:\n\n%?\n\n"
		;;      ;; ,(dw/read-file-as-string "~/Notes/Templates/Daily.org")
		;;      :clock-in :clock-resume
		;;      :empty-lines 1)
		;; ("jm" "Meeting" entry
		;;      (file+olp+datetree "~/Projects/Code/emacs-from-scratch/OrgFiles/Journal.org")
		;;      "* %<%I:%M %p> - %a :meetings:\n\n%?\n\n"
		;;      :clock-in :clock-resume
		;;      :empty-lines 1)

		;; ("w" "Workflows")
		;; ("we" "Checking Email" entry (file+olp+datetree "~/Projects/Code/emacs-from-scratch/OrgFiles/Journal.org")
		;;      "* Checking Email :email:\n\n%?" :clock-in :clock-resume :empty-lines 1)
		;; ("m" "Metrics Capture")
		;; ("mw" "Weight" table-line (file+headline "~/Projects/Code/emacs-from-scratch/OrgFiles/Metrics.org" "Weight")
		;;  "| %U | %^{Weight} | %^{Notes} |" :kill-buffer t)
		))


(defun org-mode-visual-fill ()
  (setq visual-fill-column-width 100
		visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :hook (org-mode . org-mode-visual-fill))


(setq org-fc-directories '("~/.config/emacs/plugs/org-fc/"))



;; ROAM
(use-package org-roam
  :ensure t
  :custom
  (org-roam-directory (file-truename "~/Documents/notes"))
  (org-roam-db-autosync-mode)
  (org-roam-capture-templates
   '(("d" "default" plain
      "%?"
      :if-new (file+head "${slug}.org" "#+title: ${title}\n#+Author:Adarsha Acharya")
      :unnarrowed t)
	 ;; ("p" "project" plain "* Goals\n\n%?\n\n* Tasks\n\n** TODO Add initial tasks\n\n* Dates\n\n"
	 ;; 	:if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+filetags: Project")
	 ;; 	:unnarrowed t)
	 ))
  :config
  (org-roam-setup)
  )

(provide 'org-setup)
