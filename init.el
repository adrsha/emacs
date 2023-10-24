;; Removing wayland input lags !!IMPORTANT
(setq-default pgtk-wait-for-event-timeout 0)

(setq frame-inhibit-implied-resize t) ;; Supposed to hasten startup

;; No mode line
(setq-default mode-line-format nil)

;; No startup  screen
(setq inhibit-startup-screen t)

;; No random messages
(setq inhibit-message nil)

;; startup message
(setq server-client-instructions nil)
(setq inhibit-startup-echo-area-message t)

;; No message in scratch buffer
(setq initial-scratch-message nil)

;; Initial buffer
;;(setq initial-buffer-choice (lambda () (get-buffer-create "*dashboard*")))
(setq initial-buffer-choice nil)

;; Explicitly define a width to reduce the cost of on-the-fly computation
(setq-default display-line-numbers-width 3)

;; No frame title
(setq frame-title-format nil)

;; No file dialog
(setq use-file-dialog nil)

;; No dialog box
(setq use-dialog-box nil)

;; No popup windows
(setq pop-up-windows nil)

;; No empty line indicators
(setq indicate-empty-lines nil)

;; No cursor in inactive windows
(setq-default cursor-in-non-selected-windows nil)

;; No scroll bars
(if (fboundp 'scroll-bar-mode) (set-scroll-bar-mode nil))

;; No toolbar
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))

;; No menu bar
(menu-bar-mode -1)

;; No tabs
(setq-default indent-tabs-mode nil)

;; Tab.space equivalence
(setq-default tab-width 4)

;; Size of temporary buffers
(temp-buffer-resize-mode)
(setq-default temp-buffer-max-height 8)

;; Minimum window height
(setq-default window-min-height 1)

;; Tab behavior
(setq tab-always-indent 't)
;; (global-company-mode)
;; (define-key company-mode-map [remap indent-for-tab-command]
;;   #'company-indent-or-complete-common)

;; Indenting style
(setq c-default-style "linux"
      c-basic-offset 'tab-width)

;; scroll if reached the end only
(setq-default scroll-margin 0 )

;;When you scroll down, and up again, point should end up at the same position you started out with
(setq scroll-preserve-screen-position t)

(setq auto-window-vscroll nil)
;; Scroll line wise
(setq-default scroll-step 1)

;; What do these do?
(customize-set-variable 'fast-but-imprecise-scrolling t)
(customize-set-variable 'scroll-conservatively 101)

;; Moderate font lock
;; Disabling syntax highlights
;; (setq font-lock-maximum-decoration nil)

;; No limit on font lock (tree style levels)
(setq-default font-lock-maximum-size nil)

;; No line break space points
(setq-default auto-fill-mode nil)

;; Better autofill --  is nil by default
;; (setq refill-mode nil)

;; No confirmation for visiting non-existent files
(setq-default confirm-nonexistent-file-or-buffer nil)

;; Use RET to open org-mode links, including those in quick-help.org
(setq-default org-return-follows-link t)

;; Internal border / padding
;; (set-frame-parameter (selected-frame) 'internal-border-width 20)
(add-to-list 'default-frame-alist '(internal-border-width . 10 ))
(add-to-list 'default-frame-alist '(internal-show-cursor . -1))

;; Disable the line break symbols
(setf (cdr (assq 'continuation fringe-indicator-alist)) '(nil nil))

;; Links
(setq org-link-frame-setup
      '((vm . vm-visit-folder-other-frame)
        (vm-imap . vm-visit-imap-folder-other-frame)
        (gnus . org-gnus-no-new-news)
        (file . find-file)
        (wl . wl-other-frame)))

;; Disable the $ symbol too
(set-display-table-slot standard-display-table 'truncation ?\ )

;; Disable fringe
(fringe-mode '(0 . 0))

;; Display Minimum warnings
(setq-default warning-minimum-level :emergency)

;; Display line numbers globally
(global-display-line-numbers-mode t)

;; Disable line number mode in a few situations
(dolist (mode '(org-mode-hook org-agenda-mode term-mode-hook dired-mode-hook shell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; Revert buffers when the underlying file has changed
(global-auto-revert-mode 1)

;; Revert Dired and other buffers
(setq-default global-auto-revert-non-file-buffers t)

;; Enable recentf
(require 'recentf)
(recentf-mode 1)

;; Silence compiler warnings as they can be pretty disruptive
(if (boundp 'comp-deferred-compilation)
    (setq-default comp-deferred-compilation nil)
  (setq-default native-comp-deferred-compilation nil))

(setq-default native-comp-async-report-warnings-errors nil)

;; In noninteractive sessions, prioritize non-byte-compiled source files to
;; prevent the use of stale byte-code. Otherwise, it saves us a little IO time
;; to skip the mtime checks on every *.elc file.
(setq-default load-prefer-newer noninteractive)

;; No ugly button for checkboxes
(setq-default widget-image-enable nil)

;; Disable Blink cursor
(blink-cursor-mode -1)

;; Disable signatures and stuff on minibuf
(global-eldoc-mode 1)

;; Save my last place
(save-place-mode 1)

;; Move customization variables to a separate file and load it
(setq-default custom-file (locate-user-emacs-file "custom-vars.el"))
(load custom-file 'noerror 'nomessage)

;; Visual line mode :
(set-default 'truncate-lines t)

;; Raise undo-limit to 80Mb
(setq-default undo-limit 80000000)

;; Autosave true
(setq-default auto-save-default t)

;; ease of life
(fset 'yes-or-no-p 'y-or-n-p)

;; itterate through CamelCase words
;; (global-subword-mode 1)

(setq-default delete-by-moving-to-trash t); Delete files to trash

(setq-default delete-selection-mode t)

;; Disabling message logs
;; (setq-default message-log-max nil)

;;(setq max-mini-window-height 1) ; Don't let echo area grow;;
(setq resize-mini-windows t)

;; disable the delays
(setq-default show-paren-delay 0.0)
(setq-default eldoc-idle-delay 0.0)
(setq-default highlight-indent-guides-delay 0.01)

;; Disable default css colors
(setq-default css-fontify-colors nil)

;; empty line at the end
(setq require-final-newline t)

;; Auto executable if consists a shebang
(add-hook 'after-save-hook #'executable-make-buffer-file-executable-if-script-p)

;; This HAS to come before (require 'org)
(setq org-emphasis-regexp-components
      '("     ('\"{“”"
        "-   .,!?;''“”\")}/\\“”"
        "    \r\n,"
        "."
        1))

;; Disable the headerline for org src and org capture
(add-hook 'org-src-mode-hook '(lambda () (interactive) (setq header-line-format 'nil)))
(add-hook 'org-capture-mode-hook '(lambda () (interactive) (setq header-line-format 'nil)))

;; Disable echoing keystrokes
(setq-default echo-keystrokes 0)
(setq-default evil-echo-state nil)

(defun org-hugo--advice-silence-messages (orig-fun &rest args)
  "Advice function that silences all messages in ORIG-FUN."
  (let ((inhibit-message t)      ;Don't show the messages in Echo area
        (message-log-max nil))   ;Don't show the messages in the *Messages* buffer


    (dolist (fn '(org-babel-exp-src-block write-region)
                (advice-add fn :around #'org-hugo--advice-silence-messages))
      (apply orig-fun args))))

;; Custom Windows with custom vehaviours
(add-to-list 'display-buffer-alist '("\\*helpful.*"
                                     (display-buffer-in-side-window)
                                     (side . right)
                                     (window-width . 50)
                                     ))
(add-to-list 'display-buffer-alist '("\\.*embark.*"
                                     (display-buffer-in-side-window)
                                     (side . top)
                                     (window-width . 30)
                                     ))
(add-to-list 'display-buffer-alist '("\\.*lsp-ref.*"
                                     (display-buffer-in-side-window)
                                     (side . top)
                                     (window-width . 30)
                                     ))

(add-to-list 'load-path "~/.config/emacs/packages/")
(add-to-list 'load-path "~/.config/emacs/blink-search/")

(require 'package)

(setq package-user-dir (expand-file-name "elpa" user-emacs-directory))

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/")) ;; ELPA and NonGNU ELPA are default in Emacs28

(package-initialize)
(setq package-enable-at-startup nil)

(require 'use-package)
(setq use-package-always-ensure t)
(setq use-package-verbose nil)

(defvar bgcolor "#11111b"
  "The normal background of emacs.")
(defvar grim-bgcolor "#383945"
  "The darker background of emacs.")
(defvar darker-bgcolor "#0D0D15"
  "The darker background of emacs.")
(defvar darkest-bgcolor "#0B0B11"
  "The darker background of emacs.")
(defvar dim-fgcolor "#6C7096"
  "The calm foreground of emacs.")
(defvar calm-fgcolor "#BAC2DE"
  "The calm foreground of emacs.")
(defvar mauve-color "#cba6f7"
  "The blue color for emacs.")
(defvar lavender-color "#b4befe"
  "The blue color for emacs.")
(defvar blue-color "#90b6f3"
  "The blue color for emacs.")
(defvar pink-color "#cba6f7"
  "The pink color for emacs.")
(defvar red-color "#f38ba8"
  "The red color for emacs.")
(defvar teal-color "#a6e3a1"
  "The pink color for emacs.")
(defvar grim-fgcolor "#232338"
  "The calm foreground of emacs.")

(defun delete-window-or-frame (&optional window frame force)
  (interactive)
  (if (= 1 (length (window-list frame)))
      (delete-frame frame force)
    (delete-window window)))

(defun clear ()
  (interactive)
  ;; (evil-ex-nohighlight)
  (redraw-display)
  ;; (posframe-hide-all)
  ;; (evil-mc-undo-all-cursors)
  ;; (evil-force-normal-state)
  )

(defun configure-evil-ins ()
  "Default evil ins key"
  (evil-escape-mode 1))
(add-hook 'evil-insert-state-entry-hook #'configure-evil-ins)
(add-hook 'minibuffer-mode-hook #'(lambda () (interactive) (evil-escape-mode 1) ))


;; To prevent the visual mode lag:
(defun configure-evil-exit-ins ()
  "Default evil ins key"
  (evil-escape-mode -1))
(add-hook 'evil-visual-state-entry-hook #'configure-evil-exit-ins)

(defcustom my-skippable-buffers '("*Warnings*" "*Messages*" "*scratch*" "*Help*" "*Completions*" "*flymake-posframe-buffer" "*Async-native-compile-log*" )
  "Buffer names ignored by `my-next-buffer' and `my-previous-buffer'."
  :type '(repeat string))

(defun my-change-buffer (change-buffer)
  "Call CHANGE-BUFFER until current buffer is not in `my-skippable-buffers'."
  (let ((initial (current-buffer)))
    (funcall change-buffer)
    (let ((first-change (current-buffer)))
      (catch 'loop
        (while (member (buffer-name) my-skippable-buffers)
          (funcall change-buffer)
          (when (eq (current-buffer) first-change)
            (switch-to-buffer initial)
            (throw 'loop t)))))))

(defun open-current-file-in-vim ()
  (interactive)
  (async-shell-command
   (format "foot nvim +%d %s"
           (+ (if (bolp) 1 0) (count-lines 1 (point)))
           (shell-quote-argument buffer-file-name))))


(defun my-next-buffer ()
  "Variant of `next-buffer' that skips `my-skippable-buffers'."
  (interactive)
  (my-change-buffer 'next-buffer))

(defun my-previous-buffer ()
  "Variant of `previous-buffer' that skips `my-skippable-buffers'."
  (interactive)
  (my-change-buffer 'previous-buffer))

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

(defun agenda-color-char ()
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "" nil t)
      (put-text-property (match-beginning 0) (match-end 0)
                         'face '(:height 220 :foreground "gold2" :bold t)))))

(defun ex/rename-current-buffer-file ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let* ((name (buffer-name))
         (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let* ((dir (file-name-directory filename))
             (new-name (read-file-name "New name: " dir)))
        (cond ((get-buffer new-name)
               (error "A buffer named '%s' already exists!" new-name))
              (t
               (let ((dir (file-name-directory new-name)))
                 (when (and (not (file-exists-p dir)) (yes-or-no-p (format "Create directory '%s'?" dir)))
                   (make-directory dir t)))
               (rename-file filename new-name 1)
               (rename-buffer new-name)
               (set-visited-file-name new-name)
               (set-buffer-modified-p nil)
               (when (fboundp 'recentf-add-file)
                 (recentf-add-file new-name)
                 (recentf-remove-if-non-kept filename))
               (message "File '%s' successfully renamed to '%s'" name (file-name-nondirectory new-name))))))))

(defun ex/google-this ()
  "Google the selected region if any, display a query prompt otherwise."
  (interactive)
  (browse-url
   (concat
    "http://www.google.com/search?ie=utf-8&oe=utf-8&q="
    (url-hexify-string (if mark-active
                           (buffer-substring (region-beginning) (region-end))
                         (read-string "Google: "))))))

(defun ex/org-schedule-tomorrow ()
  "Org Schedule for tomorrow (+1d)."
  (interactive)
  (org-schedule t "+1d"))

;; Search and replace pair-by-pair
(defun batch-replace-strings (replacement-alist)
  "Prompt user for pairs of strings to search/replace, then do so in the current buffer"
  (interactive (list (batch-replace-strings-prompt)))
  (dolist (pair replacement-alist)
    (save-excursion
      (replace-string (car pair) (cdr pair)))))

(defun batch-replace-strings-prompt ()
  "prompt for string pairs and return as an association list"
  (let (from-string
        ret-alist)
    (while (not (string-equal "" (setq from-string (read-string "String to search (RET to stop): "))))
      (setq ret-alist
            (cons (cons from-string (read-string (format "Replace %s with: " from-string)))
                  ret-alist)))
    ret-alist))

;; Dired fixes
(setq dired-use-ls-dired nil)
(setq dired-kill-when-opening-new-dired-buffer t)
;; DIRed
(setq dired-listing-switches "-Al --group-directories-first")
(setq-default dired-kill-when-opening-new-dired-buffer 't)

(defun use-betterfonts-dired ()
  "Switch the current buffer to a monospace font."
  (face-remap-add-relative 'default '(:family "Barlow Semi Condensed")))

(add-hook 'dired-mode-hook 'use-betterfonts-dired)
(add-hook 'dired-mode-hook 'dired-hide-details-mode)

(use-package evil
  :init
  (setq evil-undo-system 'undo-fu)
  (setq evil-want-keybinding nil) ;; don't load Evil keybindings in other modes
  (setq evil-want-C-i-jump nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-d-scroll t)
  (setq evil-want-fine-undo t)
  (setq evil-want-Y-yank-to-eol t)

  ;; ----- Setting cursor colors
  (setq evil-emacs-state-cursor    '("#f38ba8" box))
  (setq evil-normal-state-cursor   '("#cba6f7" box))
  (setq evil-operator-state-cursor '("#90b6f3" (bar . 6)))
  (setq evil-visual-state-cursor   '("#6C7096" box))
  (setq evil-insert-state-cursor   '("#90b6f3" (bar . 2)))
  (setq evil-replace-state-cursor  '("#f38ba8" hbar))
  (setq evil-motion-state-cursor   '("#a6e3a1" box))
  :config
  (evil-mode 1)
  ;; INITIAL BINDINGS
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)
  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal)
  (evil-define-key 'motion help-mode-map "q" 'kill-this-buffer)
  )

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package evil-nerd-commenter
  :config)

(use-package evil-escape
  :config
  (evil-escape-mode)
  :custom
  (evil-escape-key-sequence "jk")
  (evil-escape-delay 0.2))

(use-package evil-matchit
  :config
  (evil-matchit-mode 1))

(use-package evil-surround
  :config
  (global-evil-surround-mode 1)
  :after evil)

(use-package general
  :config

(global-set-key [remap next-buffer] 'my-next-buffer)
(global-set-key [remap previous-buffer] 'my-previous-buffer)

(general-def
  "C-j" 'nil
  "C-k" 'nil)

(general-def
  "M-p" 'popper-toggle-type
  "M-n" 'popper-cycle
  "M-," 'which-key-abort
  "M-S-n" 'popper-cycle-backwards
  "M-d" 'popper-kill-latest-popup
  "C-;" 'embark-become
  "C-<return>" 'embark-act
  "<escape>" 'keyboard-escape-quit)

(general-create-definer e/leader-keys
  :keymaps '(normal insert visual emacs)
  :prefix "SPC"
  :global-prefix "C-SPC"
  )


(general-create-definer e/goto-keys
  :keymaps '(normal insert)
  :prefix "g"
  :global-prefix "C-g"
  )

(e/leader-keys
  "SPC" '(execute-extended-command :which-key "  M-x  ")
  "k" '(eldoc-box-help-at-point :which-key "  hover  "))

(e/leader-keys
  "c"  '(:ignore t :which-key "󰅱  code  ")
  "cr"  '(eglot-rename :which-key "󰑕  rename symbol  ")
  "cd"  '(duplicate-dwim :which-key "  code duplicate  ")
  "ce"  '(org-ctrl-c-ctrl-c :which-key "󰅱  execute code in org  ")
  "cc"  '(format-all-buffer :which-key "  format region or buffer  ")
  "cf" '((lambda () (interactive) (indent-region (point-min) (point-max))) :wk "  format default  "))

(e/leader-keys
  "a"  '(:ignore t :which-key "  avy  ")
  "aa" '(evil-avy-goto-word-1 :which-key "󰀫  avy char  ")
  "al" '(avy-goto-line :which-key "󰂶  avy line  ")
  "am"  '(:ignore t :which-key "  avy move  ")
  "aml" '(avy-move-line :which-key "󰂶  avy move line  "))

(e/leader-keys
  "f"  '(:ignore t :which-key "󰈔  files  ")
  "ff" '(find-file :which-key "󰈞  find a file  ")
  "fr" '(consult-recent-file :which-key "󰣜  recent files  ")
  "fi" '(file-info-show :which-key "  file info  ")
  "fot" '(org-babel-tangle :which-key "󰗆  org tangle")
  "fn" '(org-roam-node-find :which-key "󰣜  find nodes  ")
  "fc"  '(:ignore t :which-key "󰈔  current file  "))

(e/leader-keys
  "o"  '(:ignore t :which-key "󰉋  open  ")
  "ow" '(crux-open-with :which-key "  open with  ")
  "od" '(dired-jump :which-key "󰉓   open dired  ")
  "oh" '(hydra-hydras/body :which-key "󰊠  open hydras  ")
  "oe" '(e/org-babel-edit :which-key "󰕪  open agendas  ")
  "oa" '(org-agenda :which-key "󰕪   open agendas  ")
  "oc" '(org-capture :which-key "󰄄   open capture  ")
  "or" '(consult-yank-from-kill-ring :which-key "  open registry and yank  ")
  "og"  '(org-roam-graph :which-key "󱁉  Open graph  ")
  "oF" '(list-faces-display :which-key " 󰙃  list faces"))


(e/leader-keys
  "i" '(:ignore t :which-key "󰡁  insert  ")
  "is" '(org-schedule :which-key "󰾖   insert schedule  ")
  "id" '(org-deadline :which-key "󰾕   insert deadline  ")
  "il" '(org-insert-link :which-key "   insert link  ")
  "in" '(org-roam-node-insert :which-key "   insert link  ")
  "ic" '(:ignore t :which-key "  insert cursor  ")
  "icI" '(evil-mc-make-cursor-in-visual-selection-beg :which-key "  insert cursor at the beginning  ")
  "icA" '(evil-mc-make-cursor-in-visual-selection-end :which-key "  insert cursor at the end  ")
  "icc" '(hydra-insert-cursor/body :which-key "  insert cursor hydra  ")
  "it" '(org-insert-time-stamp :which-key "   insert time stamp   ")
  )

(e/leader-keys
  "b"  '(:ignore t :which-key "  buffer navigation  ")
  "bd" '(kill-buffer-and-window :which-key "󰆴  kill the current buffer and window  ")
  "bk" '(kill-some-buffers :which-key "󰛌  kill all other buffers and windows  ")
  "bn" '(next-buffer :which-key "󰛂   switch buffer  ")
  "bp" '(previous-buffer :which-key "󰛁   switch buffer  ")
  "bb" '(consult-buffer :which-key "󰕰  view buffers  "))


(e/leader-keys
  "s"  '(:ignore t :which-key "  search  ")
  "si" '(nerd-icons-insert :which-key "󰭟   search for icons  ")
  "ss" '(consult-line :which-key "󰱼  line search  ")
  "srg" '(consult-ripgrep :which-key "󰟥   search with rg  ")
  "sd" '(dictionary-search :which-key "  search in dictionary  "))

(e/leader-keys
  "e"  '(:ignore t :which-key "󰈈   evaluate  ")
  "eb" '(eval-buffer :which-key "󰷊  evaluate buffer  ")
  "ee" '(eval-last-sexp :which-key "󰷊  evaluate last expression  ")
  "er" '(eval-region :which-key "󰨺  evaluate region  "))

(e/leader-keys
  "p"  '(:ignore t :which-key "󰅱  project  ")
  "pr"  '(projectile-recentf :which-key "󰈞  recentf  ")
  "pv"  '(:ignore t :which-key "󰅱  view  ")
  "pvc"  '(projectile-vc :which-key "󰈞  view changes  ")
  "pvd"  '(projectile-browse-dirty-projects :which-key "󰈞  view dirty projects  ")
  "ps"  '(:ignore t :which-key "󰅱  switch  ")
  "psp"  '(projectile-switch-open-project :which-key "󰅱  switch project  "))

(e/leader-keys
  "h"  '(:ignore t :which-key "󰞋   help  ")
  "ht" '(helpful-at-point :which-key "  describe this  ")
  "hF" '(describe-face :which-key "󱗎  describe face  ")
  "hf" '(helpful-function :which-key "󰯻  describe function  ")
  "hh" '(devdocs-lookup :which-key "󰯻  describe function  ")
  "hb" '(embark-bindings :which-key "󰌌  describe bindings  ")
  "hk" '(helpful-key :which-key "󰯻  describe this key  ")
  "hv" '(helpful-variable :which-key "  describe variable  ")
  "hi" '(consult-imenu :which-key "󰯻  describe this key  ")
  "hr" '(:ignore t :which-key "󱍸  reload  ")
  "hrb" '(revert-buffer-quick :which-key "󰄸  reload buffer  ")
  "hrr" '((lambda () (interactive) (load-file "~/.config/emacs/init.el")) :wk "  Reload emacs config  "))

(e/leader-keys
  "t"  '(:ignore t :which-key "   toggles/switches  ")
  "tt"  '(toggle-truncate-lines :which-key "󰖶  toggle word wrap mode  ")
  "tv" '(visual-line-mode :which-key "  visual line mode ")
  "tR" '(read-only-mode :which-key "󰑇  read only mode  ")
  "tc"  '(:ignore t :which-key "󰮫  toggle completion  ")
  "tce" '((lambda () (interactive) (setq-default corfu-auto t) (corfu-mode 1)) :wk "   enable  ")
  "tcd" '((lambda () (interactive) (setq-default corfu-auto nil) (corfu-mode 1)) :wk "   disable  ")
  "tr"  '(org-roam-buffer-toggle :which-key "  Roam Buffer  ")
  "tm"  '(minimap-mode :which-key "󰍍  minimap toggles  "))

(e/goto-keys
  "n"  '(flycheck-next-error :which-key " next error")
  "p"  '(flycheck-previous-error :which-key " next error")
  )

(general-def
  :keymaps 'evil-normal-state-map
  "C-u" #'evil-scroll-up
  "C-d" #'evil-scroll-down
  "C-s" (lambda () (interactive) (evil-ex "%s/"))
  "C-S-s" 'iedit-mode
  "C-l" 'clear
  "RET" 'org-open-at-point-global
  "M-k" 'drag-stuff-up
  "M-j" 'drag-stuff-down
  "M-h" 'drag-stuff-left
  "M-l" 'drag-stuff-right
  "C-/" #'consult-line-multi
  "C-j" #'evil-mc-make-and-goto-next-match
  "C-S-j" #'evil-mc-skip-and-goto-next-match
  "C-k" #'evil-mc-make-and-goto-prev-match
  "C-S-k" #'evil-mc-skip-and-goto-prev-match
  "C-S-p" #'evil-mc-undo-last-added-cursor
  "C-a" #'evil-mc-make-all-cursors
  "gcc" #'evilnc-comment-or-uncomment-lines
  "gca" (lambda () (interactive) (comment-indent) (just-one-space) (evil-append-line 1))
  )

(general-def
  :keymaps 'evil-insert-state-map
  "C-h" 'nil
  "C-k" 'corfu-previous
  "C-j" 'corfu-next
  ;; "C -." 'yas-expand
  "C-l" 'completion-at-point
  "C-e" 'corfu-quit
  "C-f" 'find-file-at-point
  )

(general-def
  :keymaps 'evil-visual-state-map
  "gc" #'evilnc-comment-or-uncomment-lines
  ;; "C-k" 'corfu-previous
  ;; "C-j" 'corfu-next
  ;; "C -." 'yas-expand
  )

(general-def
  :keymaps 'evil-motion-state-map
  "K" 'nil
  )

(general-def
  :keymaps 'org-mode-map
  "C-h" 'nil
  "C-S-h" 'nil
  )

(evil-collection-define-key 'normal 'dired-mode-map
  "l" 'dired-find-alternate-file
  "h" 'dired-up-directory
  "c" 'dired-create-empty-file
  "Q" 'kill-buffer-and-window
  )

(general-def
  :keymaps 'vertico-map
  "C-l" '(lambda () (interactive) (vertico-insert) )
  "C-S-l" '(lambda () (interactive) (vertico-insert) (minibuffer-force-complete-and-exit))
  "C-j" #'vertico-next
  "C-k" #'vertico-previous
  "C-h" #'vertico-directory-up
  )

(general-def
  :keymaps 'treemacs-mode-map
  "C-u" #'evil-scroll-up
  "C-l" '(lambda () (interactive) ((evil-ex-nohighlight)))
  "L" #'treemacs-root-down
  "M-\\" #'treemacs-select-window
  "M-a" #'treemacs-add-project
  "M-d" #'treemacs-remove-project-from-workspace
  )

;; NOTE: =Information on general=

;; INFO:  Mode specific maps
;; (general-def org-mode-map
;;   "C-c C-q" 'counsel-org-tag
;;   ;; ...
;;   )

;; INFO: normal maps
;; (general-define-key
;;  "M-x" 'amx
;;  "C-s" 'counsel-grep-or-swiper)

;; INFO: prefix
;; (general-define-key
;;  :prefix "C-c"
;;  ;; bind "C-c a" to 'org-agenda
;;  "a" 'org-agenda
;;  "b" 'counsel-bookmark
;;  "c" 'org-capture)

;; INFO: Swap!
;; (general-swap-key nil 'motion
;;   ";" ":")
)

(use-package catppuccin-theme
  :config
  (setq catppuccin-flavor 'mocha) ;; or 'latte, 'macchiato, or 'mocha
  (load-theme 'catppuccin :no-confirm)

  ;; Customization
(catppuccin-set-color 'rosewater "#f5e0dc")
(catppuccin-set-color 'flamingo "#f2cdcd")
(catppuccin-set-color 'pink "#f5c2e7")
(catppuccin-set-color 'mauve "#cba6f7")
(catppuccin-set-color 'red "#f38ba8")
(catppuccin-set-color 'maroon "#eba0ac")
(catppuccin-set-color 'peach "#fab387")
(catppuccin-set-color 'yellow "#f9e2af")
(catppuccin-set-color 'green "#a6e3a1")
(catppuccin-set-color 'teal "#94e2d5")
(catppuccin-set-color 'sky "#89dceb")
(catppuccin-set-color 'sapphire "#74c7ec")
(catppuccin-set-color 'blue "#89b4fa")
(catppuccin-set-color 'lavender "#b4befe")
(catppuccin-set-color 'text "#cdd6f4")
(catppuccin-set-color 'subtext1 "#bac2de")
(catppuccin-set-color 'subtext0 "#a6adc8")
(catppuccin-set-color 'overlay2 "#9399b2")
(catppuccin-set-color 'overlay1 "#7f849c")
(catppuccin-set-color 'overlay0 "#6c7086")
(catppuccin-set-color 'surface2 "#585b70")
(catppuccin-set-color 'surface1 "#45475a")
(catppuccin-set-color 'surface0 "#313244")
(catppuccin-set-color 'mantle "#0E0E16")
(catppuccin-set-color 'crust "#0B0B11")
(catppuccin-set-color 'base "#11111B")

  (catppuccin-reload)
  )

(use-package which-key
  :config
  ;; Set the time delay (in seconds) for the which-key popup to appear. A value of
  ;; zero might cause issues so a non-zero value is recommended.
  (setq which-key-idle-delay 0.5)

  ;; Set the maximum length (in characters) for key descriptions (commands or
  ;; prefixes). Descriptions that are longer are truncated and have ".." added.
  ;; This can also be a float (fraction of available width) or a function.
  (setq which-key-max-description-length 27)

  ;; Use additional padding between columns of keys. This variable specifies the
  ;; number of spaces to add to the left of each column.
  (setq which-key-add-column-padding 0)

  ;; The maximum number of columns to display in the which-key buffer. nil means
  ;; don't impose a maximum.
  (setq which-key-max-display-columns nil)

  ;; Set the separator used between keys and descriptions. Change this setting to
  ;; an ASCII character if your font does not show the default arrow. The second
  ;; setting here allows for extra padding for Unicode characters. which-key uses
  ;; characters as a means of width measurement, so wide Unicode characters can
  ;; throw off the calculation.
  (setq which-key-separator "  " )

  ;; Set the prefix string that will be inserted in front of prefix commands
  ;; (i.e., commands that represent a sub-map).
  (setq which-key-prefix-prefix " " )

  ;; Set the special keys. These are automatically truncated to one character and
  ;; have which-key-special-key-face applied. Disabled by default. An example
  ;; setting is
  ;; (setq which-key-special-keys '("SPC" "TAB" "RET" "ESC" "DEL"))
  (setq which-key-special-keys nil)

  ;; Show the key prefix on the left, top, or bottom (nil means hide the prefix).
  ;; The prefix consists of the keys you have typed so far. which-key also shows
  ;; the page information along with the prefix.
  (setq which-key-show-prefix 'nil)

  ;; Set to t to show the count of keys shown vs. total keys in the mode line.
  (setq which-key-show-remaining-keys nil)
  (which-key-mode))

(use-package try)

(use-package markdown-mode)

(use-package gcmh
  :diminish gcmh-mode
  :config
  (setq gcmh-idle-delay 5
        gcmh-high-cons-threshold (* 16 1024 1024))  ; 16mb
  (gcmh-mode 1))

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-percentage 0.1))) ;; Default value for `gc-cons-percentage'

(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs ready in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

(use-package super-save
  :diminish super-save-mode
  :defer 2
  :config
  (setq super-save-auto-save-when-idle t
        super-save-idle-duration 5 ;; after 5 seconds of not typing autosave
        super-save-triggers ;; Functions after which buffers are saved (switching window, for example)
        '(evil-window-next evil-window-prev balance-windows other-window next-buffer previous-buffer)
        super-save-max-buffer-size 10000000)
  (super-save-mode +1))

;; After super-save autosaves, wait __ seconds and then clear the buffer. I don't like
;; the save message just sitting in the echo area.
(defun clear-echo-area-timer ()
  (run-at-time "2 sec" nil (lambda () (message " "))))
(advice-add 'super-save-command :after 'clear-echo-area-timer)

(use-package saveplace
  :init (setq save-place-limit 100)
  :config (save-place-mode))

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
          ("DONE"       success bold)
          ("HACK"       font-lock-constant-face bold)
          ("WARN"       warning bold)
          ("REVIEW"     font-lock-keyword-face bold)
          ("NOTE"       success bold)
          ("DEPRECATED" shadow bold))))

(use-package avy)

(use-package devdocs)

;; Enable vertico
(use-package vertico
  :init
  (vertico-mode)
  ;; Different scroll margin
  (setq vertico-scroll-margin 5)

  ;; Show more candidates
  (setq vertico-count 10)

  ;; Grow and shrink the Vertico minibuffer
  (setq vertico-resize t)

  ;; Optionally enable cycling for `vertico-next' and `vertico-previous'.
  (setq vertico-cycle t)
  )
(use-package savehist
  :init
  (savehist-mode))

(use-package emacs
  :init
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Emacs 28: Hide commands in M-x which do not work in the current mode.
  ;; Vertico commands are hidden in normal buffers.
  ;; (setq read-extended-command-predicate
  ;;       #'command-completion-default-include-p)

  ;; Enable recursive minibuffers
  (setq enable-recursive-minibuffers t))

(use-package consult
  :init
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  (advice-add #'register-preview :override #'consult-register-window)
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  :config
  (add-to-list 'consult-buffer-filter "\*.*\*")

  ;; Allowing single key press to begin asynchorous searches like consult-grep
  (setq consult-async-min-input 1)

  (consult-customize
   consult-theme consult-buffer :preview-key '(:debounce 0.2 any)
   consult-recent-file :preview-key "C-h"
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any))

  (setq consult-narrow-key "<") ;; "C-+"
  )

(use-package marginalia)
(marginalia-mode)

;; Optionally use the `orderless' completion style.
(use-package orderless
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-consult-dispatch orderless-affix-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-cycle-threshold 0
        completion-category-overrides '((file (styles partial-completion)))))

(use-package corfu
  :custom
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-separator ?\s)          ;; Orderless field separator
  (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  (corfu-quit-no-match t)      ;; Never quit, even if there is no match
  (corfu-preview-current nil)    ;; Disable current candidate preview
  (corfu-preselect 'first)      ;; Preselect the prompt
  (corfu-on-exact-match nil)     ;; Configure handling of exact matches
  (corfu-scroll-margin 5)        ;; Use scroll margin
  (corfu-minimum-width 100)        ;; Use scroll margin
  (corfu-maximum-width 190)        ;; Use scroll margin
  (corfu-auto-prefix 1)
  (corfu-auto-delay 0.3)
  (corfu-popupinfo-delay '(2.0 . 1.0))

  :config
  (corfu-popupinfo-mode 1)
  (corfu-history-mode 1))

(setq corfu--frame-parameters '((no-accept-focus . t)
                                (no-focus-on-map . t)
                                (min-width . t)
                                (min-height . t)
                                (border-width . 0)
                                (child-frame-border-width . 10)
                                (left-fringe . 0)
                                (right-fringe . 0)
                                (vertical-scroll-bars)
                                (horizontal-scroll-bars)
                                (menu-bar-lines . 0)
                                (tool-bar-lines . 0)
                                (tab-bar-lines . 0)
                                (no-other-frame . t)
                                (unsplittable . t)
                                (undecorated . t)
                                (cursor-type)
                                (no-special-glyphs . t)
                                (desktop-dont-save . t)))

;; Add extensions
(use-package cape
  :init
  ;; Add `completion-at-point-functions', used by `completion-at-point'.
  (add-to-list 'completion-at-point-functions #'cape-dabbrev 5)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-history)
  ;; (add-to-list 'completion-at-point-functions #'cape-keyword)
  ;; (add-to-list 'completion-at-point-functions #'cape-elisp-symbol)
  (add-to-list 'completion-at-point-functions #'cape-elisp-block)
  ;; (add-to-list 'completion-at-point-functions #'cape-line)
  )

(add-hook 'eshell-mode-hook
          (lambda ()
            (setq corfu-auto t)                 ;; Enable auto completion
            (setq-local corfu-auto nil)
            (corfu-mode)))

(add-hook 'prog-mode-hook
          (lambda ()
            (setq corfu-auto t)                 ;; Enable auto completion
            (corfu-mode)))

(add-hook 'org-mode-hook
          (lambda ()
            (setq corfu-auto nil)                 ;; Enable auto completion
            (corfu-mode)))

(use-package undo-fu)
(use-package undo-fu-session
  :config
  (setq undo-fu-session-incompatible-files '("/COMMIT_EDITMSG\\'" "/git-rebase-todo\\'")))

(undo-fu-session-global-mode)

(use-package helpful
  :config
  (setq counsel-describe-function-function #'helpful-callable)
  (setq counsel-describe-variable-function #'helpful-variable))

(use-package valign
  :config
  (setq valign-fancy-bar nil)
  (add-hook 'org-mode-hook #'valign-mode))

(use-package posframe)

(use-package vertico-posframe
  :after vertico
  :init
  (add-hook 'posframe-mode-hook #'turn-off-evil-mode nil)
  :config
  (setq vertico-posframe-border-width 40)
  (setq vertico-multiform-commands
        '((consult-line
           posframe
           (vertico-posframe-poshandler . posframe-poshandler-frame-bottom-center)
           ;; NOTE: This is useful when emacs is used in both in X and
           ;; terminal, for posframe do not work well in terminal, so
           ;; vertico-buffer-mode will be used as fallback at the
           ;; moment.
           (vertico-posframe-fallback-mode . vertico-buffer-mode))
          (t posframe)))
  (vertico-multiform-mode 1))
(vertico-posframe-mode 1)

(require 'which-key-posframe)
(which-key-posframe-mode)
(setq which-key-posframe-poshandler 'posframe-poshandler-frame-bottom-center)
(setq which-key-posframe-border-width 20)

(use-package eldoc-box)
(setq eldoc-box-frame-parameters '((left . -1)
                                   (top . -1)
                                   (width  . 0)
                                   (height  . 0)
                                   (no-accept-focus . t)
                                   (no-focus-on-map . t)
                                   (min-width  . 0)
                                   (min-height  . 0)
                                   (internal-border-width . 30)
                                   (vertical-scroll-bars . nil)
                                   (horizontal-scroll-bars . nil)
                                   (right-fringe . 3)
                                   (left-fringe . 3)
                                   (menu-bar-lines . 0)
                                   (tool-bar-lines . 0)
                                   (line-spacing . 0)
                                   (unsplittable . t)
                                   (undecorated . t)
                                   (visibility . nil)
                                   (mouse-wheel-frame . nil)
                                   (no-other-frame . t)
                                   (cursor-type . nil)
                                   (inhibit-double-buffering . t)
                                   (drag-internal-border . t)
                                   (no-special-glyphs . t)
                                   (desktop-dont-save . t)
                                   (tab-bar-lines . 0)
                                   (tab-bar-lines-keep-state . 1)))

(advice-add 'eldoc-display-in-echo-area :override #'do-nothing-function )
(defun do-nothing-function (docs _interactive)
  'ignore)

(use-package nerd-icons
  :config
  :if (display-graphic-p))

(use-package all-the-icons
  :config
  (setq all-the-icons-scale-factor 0.9)
  :if (display-graphic-p))

(use-package all-the-icons-completion)

(use-package all-the-icons-dired
  :hook
  (dired-mode . all-the-icons-dired-mode))

(all-the-icons-completion-mode)

(use-package drag-stuff
  :config
  (drag-stuff-global-mode 1))

(use-package hydra
  :config
  (setq hydra-hint-display-type 'posframe)
  (setq hydra-posframe-show-params `(:poshandler posframe-poshandler-window-bottom-center
                                                 :internal-border-width 40
                                                 :internal-border-color "#111916"
                                                 :background-color "#111916"
                                                 :left-fringe 0
                                                 :right-fringe 0)))

(defhydra hydra-hydras (:color teal)
  "
           ^Hydras
^———————————————^
 ^_t_: Toggles   ^_o_: Org
 ^_w_: Window    ^_a_: Org-table
 ^_q_: Quit
    "
  ("t" hydra-toggle/body nil)
  ("w" hydra-window/body nil)
  ("o" hydra-org/body nil)
  ("a" hydra-org-table/body nil)
  ("q" nil nil)
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

  ^_l_: Org-Toggle link display
  ^_c_: Org-cycle
  ^_s_: Insert Schedule
  ^_q_: Quit

  "
  ("s" org-schedule nil)
  ("l" org-toggle-link-display nil)
  ("c" org-cycle-global nil)
  ("q" nil nil))

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
  ("q" nil nil))


(defhydra hydra-window (:hint nil)
  "
      Movement      ^Split^            ^Switch^        ^Resize^
      —————————————————————————————
      _M-<left>_  <   _/_ vertical      _b_uffer        _<left>_  <
      _M-<right>_ >   _-_ horizontal    _f_ind file     _<down>_  ↓
      _M-<up>_    ↑   _m_aximize        _s_wap          _<up>_    ↑
      _M-<down>_  ↓   _c_lose           _[_backward     _<right>_ >
      _q_uit          _e_qualize        _]_forward     ^
      ^               ^               _K_ill         ^
      ^               ^                  ^             ^
      "
  ;; Movement
  ("M-<left>" windmove-left)
  ("M-<down>" windmove-down)
  ("M-<up>" windmove-up)
  ("M-<right>" windmove-right)

  ;; Split/manage
  ("-" jib/split-window-vertically-and-switch)
  ("/" jib/split-window-horizontally-and-switch)
  ("c" evil-window-delete)
  ("d" evil-window-delete)
  ("m" delete-other-windows)
  ("e" balance-windows)

  ;; Switch
  ("b" counsel-switch-buffer)
  ("f" counsel-find-file)
  ("P" project-find-file)
  ("s" ace-swap-window)
  ("[" previous-buffer)
  ("]" next-buffer)
  ("K" kill-this-buffer)

  ;; Resize
  ("<left>" windresize-left)
  ("<right>" windresize-right)
  ("<down>" windresize-down)
  ("<up>" windresize-up)

  ("q" nil))

(defhydra hydra-org-table ()
  "
      _c_ insert col    _v_ delete col    Move col: _h_, _l_
      _r_ insert row    _d_ delete row    Move row: _j_, _k_
      _n_ create table  _i_ create hline
      _u_ undo
      _q_ quit

      "
  ("n" org-table-create nil)
  ("c" org-table-insert-column nil)
  ("r" org-table-insert-row nil)
  ("v" org-table-delete-column nil)
  ("d" org-table-kill-row nil)
  ("i" org-table-insert-hline nil)

  ("u" undo-fu-only-undo nil)

  ("h" org-table-move-column-left nil)
  ("l" org-table-move-column-right nil)
  ("k" org-table-move-row-up nil)
  ("j" org-table-move-row-down nil)

  ("<left>" org-table-previous-field nil)
  ("<right>" org-table-next-field nil)
  ("<up>" previous-line nil)
  ("<down>" org-table-next-row nil)

  ("q" nil nil))

(use-package browse-at-remote)
(use-package file-info
  :ensure t
  :bind (("C-c d" . 'file-info-show))
  :config
  )

(use-package rainbow-mode
  :hook (org-mode prog-mode text-mode))

(use-package rainbow-delimiters
  :hook (org-mode prog-mode text-mode))

(use-package format-all)

(use-package highlight-indent-guides
  :config
  (setq highlight-indent-guides-method 'character)
  (setq highlight-indent-guides-character ?┊)
  (setq highlight-indent-guides-responsive 'top)
  )
(add-hook 'prog-mode-hook 'highlight-indent-guides-mode)

(use-package iedit)

(use-package flyspell)
(use-package flyspell-correct)

(add-to-list 'default-frame-alist '(font . "Iosevka Nerd Font Medium"))
(defun configure-font (frame)
  "Configure font given initial non-daemon FRAME.
 Intended for `after-make-frame-functions'."
  (set-face-attribute 'default nil :font "Iosevka Nerd Font Medium" :height 150)
  (set-face-attribute 'fixed-pitch nil :font "Iosevka Nerd Font Medium" :height 150)
  (set-face-attribute 'variable-pitch nil :font "Barlow SemiCondensed" :height 170)
  (set-face-attribute 'font-lock-comment-face nil :slant 'italic)
  (set-face-attribute 'font-lock-keyword-face nil :slant 'italic)
  (set-face-attribute 'line-number nil :font "Iosevka Nerd Font Bold" :height 120)
  (set-face-attribute 'link nil :background darker-bgcolor :slant 'normal  :weight 'regular :overline 'nil :underline 'nil :family "Abel")
  (set-face-attribute 'show-paren-match nil :foreground mauve-color :underline 't)
  (set-face-attribute 'show-paren-match-expression nil :background grim-bgcolor :foreground 'unspecified :inherit 'nil)
  (set-face-attribute 'help-key-binding nil :font "Barlow SemiCondensed" :weight 'semibold :background darker-bgcolor :foreground dim-fgcolor :box 'nil)
  (set-face-attribute 'header-line nil :background bgcolor :foreground dim-fgcolor)

(set-face-attribute 'evil-ex-info nil :foreground red-color :slant 'oblique :family "Barlow Semi Condensed" )
(set-face-attribute 'evil-ex-substitute-matches nil :background blue-color :foreground darker-bgcolor :strike-through 't :underline 'nil )
(set-face-attribute 'evil-ex-substitute-replacement nil :background teal-color :foreground darker-bgcolor :underline 'nil )
(set-face-attribute 'marginalia-documentation nil :family "Barlow SemiCondensed" :slant 'normal :weight 'regular)

(set-face-attribute 'org-block nil :background darker-bgcolor :font "Iosevka Nerd Font Medium")
(set-face-attribute 'org-verbatim nil :background 'unspecified :foreground dim-fgcolor :inherit 'fixed-pitch)
(set-face-attribute 'org-block-end-line nil :background darker-bgcolor)
(set-face-attribute 'org-block-begin-line nil :background darker-bgcolor)
(set-face-attribute 'org-meta-line nil :slant 'normal :height 90)
(set-face-attribute 'org-level-1 nil :height 235 :family "Barlow SemiCondensed" :weight 'regular :foreground lavender-color)
(set-face-attribute 'org-level-2 nil :height 220 :family "Barlow SemiCondensed" :weight 'regular :foreground lavender-color)
(set-face-attribute 'org-level-3 nil :height 205 :family "Barlow SemiCondensed" :weight 'regular :foreground blue-color)
(set-face-attribute 'org-level-4 nil :height 190 :family "Barlow SemiCondensed" :weight 'regular :foreground blue-color)
(set-face-attribute 'org-level-5 nil :height 190 :family "Barlow SemiCondensed" :weight 'regular :foreground blue-color)
(set-face-attribute 'org-level-6 nil :height 190 :family "Barlow SemiCondensed" :weight 'regular :foreground blue-color)
(set-face-attribute 'org-level-7 nil :height 190 :family "Barlow SemiCondensed" :weight 'regular :foreground blue-color)
(set-face-attribute 'org-level-8 nil :height 190 :family "Barlow SemiCondensed" :weight 'regular :foreground blue-color)
(set-face-attribute 'org-table nil :background darker-bgcolor :inherit 'fixed-pitch)

(set-face-attribute 'org-document-title nil :height 260 :font "Abel")
(set-face-attribute 'org-ellipsis nil :slant 'normal :foreground dim-fgcolor)
(set-face-attribute 'org-done nil :slant 'normal :strike-through 't :foreground dim-fgcolor)

(set-face-attribute 'org-agenda-date nil :font "Abel" :weight 'regular :height 200 :foreground pink-color)
(set-face-attribute 'org-agenda-date-today nil :font "Barlow SemiCondensed" :weight 'semibold :height 200 )
(set-face-attribute 'org-agenda-done nil :font "Abel" :weight 'regular :height 190 :strike-through 't)
(set-face-attribute 'org-agenda-structure nil :font "Abel" :weight 'regular :height 230 :foreground blue-color)

(set-face-attribute 'vertico-posframe nil :background darker-bgcolor :weight 'bold)
(set-face-attribute 'vertico-posframe-border nil :background darker-bgcolor :inherit 'vertico-posframe)
(set-face-attribute 'vertico-posframe-border-2 nil :background darker-bgcolor :inherit 'vertico-posframe)
(set-face-attribute 'vertico-posframe-border-3 nil :background darker-bgcolor :inherit 'vertico-posframe)
(set-face-attribute 'vertico-posframe-border-4 nil :background darker-bgcolor :inherit 'vertico-posframe)
(set-face-attribute 'vertico-posframe-border-fallback nil :background darker-bgcolor :inherit 'ansi-color-white)
(set-face-attribute 'vertico-current nil :foreground 'unspecified :weight 'bold :inherit 'org-footnote :background bgcolor)

(set-face-attribute 'minibuffer-prompt nil :inherit 'font-lock-function-call-face :weight 'bold )

(set-face-attribute 'which-key-posframe nil :background darker-bgcolor :weight 'bold)
(set-face-attribute 'which-key-posframe-border nil :background darker-bgcolor :inherit 'vertico-posframe)

(set-face-attribute 'eldoc-box-body nil :background darker-bgcolor)
(set-face-attribute 'eldoc-box-border nil :background darker-bgcolor)

(set-face-attribute 'flymake-error nil :background "#1a1a19" :foreground "#333129" :underline 'nil :weight 'bold)
(set-face-attribute 'flymake-note nil :background "#161913" :foreground "#364629" :underline 'nil :weight 'bold)
(set-face-attribute 'flymake-warning nil :background "#181812" :foreground "#434329" :underline 'nil :weight 'bold)

(set-face-attribute 'flycheck-error nil :background "#1a1a19" :foreground "#333129" :underline 'nil :weight 'bold)
(set-face-attribute 'flycheck-info nil :background "#161913" :foreground "#364629" :underline 'nil :weight 'bold)
(set-face-attribute 'flycheck-warning nil :background "#181812" :foreground "#434329" :underline 'nil :weight 'bold)

(set-face-attribute 'flycheck-error-list-error nil :foreground "#333129" :underline 'nil :weight 'bold)
(set-face-attribute 'flycheck-error-list-info nil :foreground "#364629" :underline 'nil :weight 'bold)
(set-face-attribute 'flycheck-error-list-warning nil :foreground "#434329" :underline 'nil :weight 'bold)

(set-face-attribute 'evil-ex-info nil :foreground red-color :slant 'oblique :family "Barlow Semi Condensed" )
(set-face-attribute 'evil-ex-substitute-matches nil :background blue-color :foreground darker-bgcolor :strike-through 't :underline 'nil )
(set-face-attribute 'evil-ex-substitute-replacement nil :background teal-color :foreground darker-bgcolor :underline 'nil )

(set-face-attribute 'corfu-default nil :height 150 :background darker-bgcolor :foreground dim-fgcolor :weight 'semibold :family "Iosevka Nerd Font")
(set-face-attribute 'corfu-current nil :height 150 :foreground calm-fgcolor :background bgcolor :weight 'semibold :family "Iosevka Nerd Font")
(set-face-attribute 'corfu-annotations nil :height 150 :foreground grim-fgcolor :weight 'semibold :family "Iosevka Nerd Font")

(set-face-attribute 'hydra-face-red nil :foreground red-color)
(set-face-attribute 'hydra-face-blue nil :foreground blue-color)
(set-face-attribute 'hydra-face-teal nil :foreground teal-color)
(set-face-attribute 'hydra-face-pink nil :foreground pink-color)

(set-face-attribute 'highlight-indent-guides-character-face nil :foreground grim-bgcolor)
(set-face-attribute 'highlight-indent-guides-top-character-face nil :foreground dim-fgcolor)
(set-face-attribute 'highlight-indent-guides-stack-character-face nil :foreground grim-fgcolor)

)

(add-hook 'after-make-frame-functions #'configure-font)

(defun my/org-mode/load-prettify-symbols ()
  (interactive)
  (setq prettify-symbols-alist
        '(
          ("#+begin_src" . " ")
          ("#+BEGIN_SRC" . " ")
          ("#+end_src" . " ")
          ("#+END_SRC" . " ")
          ("#+title:" . " ")
          ("#+TITLE:" . " ")
          ("#+author:" . " ")
          ("#+AUTHOR:" . " ")
          ("#+TITLE:" . " ")
          ("#+begin_example" . ?\ )
          ("#+BEGIN_EXAMPLE" . ?\ )
          ("#+end_example" . ?\ )
          ("#+END_EXAMPLE" . ?\ )
          ("#+header:" . ?\ )
          ("#+HEADER:" . ?\ )
          ("#+name:" . ?﮸)
          ("#+NAME:" . ?﮸)
          ("#+results:" . ?=)
          ("#+RESULTS:" . ?=)
          ("#+call:" . ?)
          ("#+CALL:" . ?)
          (":PROPERTIES:" . ?✱)
          (":properties:" . ?✱)
          (":PROPERTIES:"   . ?⚙)
          (":LOGBOOK:"      . ?☰) ; Same width as the gear in Ubuntu mono.
          ("CLOCK:"         . ?⧖) ; Other items in the logbook have a bullet.
          ("[-]"            . ?⊟) ; different from the other ballot icons.
          ("[#A]"           . ?🄰)
          ("[#B]"           . ?🄱)
          ("[#C]"           . ?🄲)
          ("lambda" .  "λ")
          ;; ("TODO" . "☐")
          ;; ("DONE" . "☑")
          ("[ ]" . "☐")
          ("[X]" . "☑")
          ("[-]" . "❍")
          ))
  (prettify-symbols-mode 1))

(add-hook 'org-mode-hook 'my/org-mode/load-prettify-symbols)

;; Custom pairs for electric pair
(defvar org-electric-pairs '((?/ . ?/) (?= . ?=)) "Electric pairs for org-mode.")
(electric-pair-mode 1)
(show-paren-mode 1)

;; Disable the autocompletion of pairs <>
(add-hook 'org-mode-hook (lambda () (setq-local electric-pair-inhibit-predicate `(lambda (c) (if (char-equal c ?<) t (,electric-pair-inhibit-predicate c))))))

(defun org-add-electric-pairs ()
  (interactive)

  (setq show-paren-when-point-inside-paren 't)
  (setq show-paren-highlight-openparen 'nil)
  (setq electric-pair-preserve-balance 't)
  (setq show-paren-style 'parenthesis)
  (setq electric-pair-pairs (append electric-pair-pairs org-electric-pairs))
  (setq electric-pair-text-pairs electric-pair-pairs))

(add-hook 'org-mode-hook (lambda () (org-add-electric-pairs)))

(setq read-process-output-max (* 1024 1024))

;; :completionProvider
;; :documentSymbolProvider
;; :codeLensProvider
;; :renameProvider
;; :inlayHintProvider
(setq eglot-ignored-server-capabilites '(:documentHighlightProvider
                                         :workspaceSymbolProvider
                                         :documentFormattingProvider
                                         :documentRangeFormattingProvider
                                         :documentOnTypeFormattingProvider
                                         :foldingRangeProvider))

(add-hook 'prog-mode-hook 'eglot-ensure)

(setq flymake-fringe-indicator-position 'nil)
;; Disable flymake
(add-hook 'eglot--managed-mode-hook (lambda () (flymake-mode -1)))

(use-package flycheck)
(setq flycheck-display-errors-delay 0.4)
(setq flycheck-cancel-error-display-at-point-timer 1.0)

;; Flycheck diagnosis
(add-hook 'prog-mode-hook '(lambda () (interactive)
                             (add-hook 'evil-insert-state-exit-hook '(lambda () (interactive) (flycheck-mode 1)))
                             (add-hook 'evil-insert-state-entry-hook '(lambda () (interactive) (flycheck-mode -1)))
                             ))

(use-package flycheck
  :preface

  (defun mp-flycheck-eldoc (callback &rest _ignored)
    "Print flycheck messages at point by calling CALLBACK."
    (when-let ((flycheck-errors (and flycheck-mode (flycheck-overlay-errors-at (point)))))
      (mapc
       (lambda (err)
         (funcall callback
                  (format "%s %s\n"
                          (let ((level (flycheck-error-level err)))
                            (pcase level
                              ('info (propertize (concat "  " "Info. \n")
                                                 'face 'flycheck-error-list-info))
                              ('error (propertize (concat "󰚌  " "Error! \n")
                                                  'face 'flycheck-error-list-error))
                              ('warning (propertize (concat "  " "Warning! \n")
                                                    'face 'flycheck-error-list-warning))
                              (_ level)))
                          (flycheck-error-message err)
                          )
                  :thing (or (flycheck-error-id err)
                             (flycheck-error-group err))
                  :face 'font-lock-doc-face))
       flycheck-errors)))

  (defun mp-flycheck-prefer-eldoc ()
    (add-hook 'eldoc-documentation-functions #'mp-flycheck-eldoc nil t)
    (setq eldoc-documentation-strategy 'eldoc-documentation-compose-eagerly)
    (setq flycheck-display-errors-function nil)
    (setq flycheck-help-echo-function nil))

  (use-package eglot
    :preface
    (defun mp-eglot-eldoc ()
      (setq eldoc-documentation-strategy
            'eldoc-documentation-compose-eagerly))
    :hook ((eglot-managed-mode . mp-eglot-eldoc)))

  :hook ((flycheck-mode . mp-flycheck-prefer-eldoc)))

(add-hook 'eglot--managed-mode-hook (lambda () (flycheck-mode 1)))

;; Syntax Highlighting
(require 'treesit)

(add-to-list 'treesit-language-source-alist '(bash "https://github.com/tree-sitter/tree-sitter-bash.git"))
(add-to-list 'major-mode-remap-alist '(sh-mode . bash-ts-mode))
(add-to-list 'major-mode-remap-alist '(shell-script-mode . bash-ts-mode))

(add-to-list 'treesit-language-source-alist '(python "https://github.com/tree-sitter/tree-sitter-python.git"))
(add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode))

(add-to-list 'treesit-language-source-alist '(cpp "https://github.com/tree-sitter/tree-sitter-cpp.git"))
(add-to-list 'major-mode-remap-alist '(c++-mode . c++-ts-mode))

(add-to-list 'treesit-language-source-alist '(c "https://github.com/tree-sitter/tree-sitter-c"))
(add-to-list 'major-mode-remap-alist '(c-mode . c-ts-mode))

(add-to-list 'treesit-language-source-alist '(css "https://github.com/tree-sitter/tree-sitter-css.git"))
(add-to-list 'major-mode-remap-alist '(css-mode . css-ts-mode))

(add-to-list 'treesit-language-source-alist '(html "https://github.com/tree-sitter/tree-sitter-html.git"))
(add-to-list 'major-mode-remap-alist '(html-mode . html-ts-mode))

(setq treesit-font-lock-level 4)

(use-package web-mode
  :defer t
  :init
  (add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode)) ;; Open .html files in web-mode
  :config
  (setq web-mode-enable-current-element-highlight t
        web-mode-enable-current-column-highlight t)

  :general
  (general-def
    :prefix ","
    :states 'motion
    :keymaps 'web-mode-map
    "" nil
    "i" '(web-mode-buffer-indent :which-key "web mode indent")
    "c" '(web-mode-fold-or-unfold :which-key "web mode toggle fold")
    ))

(require 'org-tempo)

;; ShortCuts
(add-to-list 'org-structure-template-alist '("sh" . "src shell"))
(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
(add-to-list 'org-structure-template-alist '("py" . "src python :results output "))
(add-to-list 'org-structure-template-alist '("cpp" . "src C++ :results verbatim \n\n  #include <iostream>\n  using namespace std;\n\n  int main(){\n    return 0;\n}"))

;; Indentation
(setq org-startup-folded 'nil)
(setq org-edit-src-content-indentation 2)
(setq org-src-preserve-indentation nil)

(with-eval-after-load 'org
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((C . t)
     (emacs-lisp . t)
     (org . t)
     (shell . t)
     (awk . t)
     (python . t)))

  (push '("conf-unix" . conf-unix) org-src-lang-modes))

(setq org-confirm-babel-evaluate nil)

(defun e/org-babel-edit()
  "Edit src block with lsp support by tangling the block and
 then setting the org-edit-special buffer-file-name to the
 absolute path. Finally load eglot."
  (interactive)
  (setq mb/tangled-file-name (expand-file-name (assoc-default :tangle (nth 2 (org-babel-get-src-block-info)))))
  (org-babel-tangle '(4))
  (setq-local buffer-file-name mb/tangled-file-name)
  (eglot-ensure))

(setq org-todo-keywords
      '(
        (sequence "IDEA(i)" "TODO(t)" "STARTED(s)" "NEXT(n)" "WAITING(w)" "|" "DONE(d)")
        (sequence "|" "CANCELED(c)" "SOMEDAY(f)")
        ))

(setq org-capture-templates
      `(("t" "Tasks / Projects")
        ("tt" "Task" entry (file+olp "~/Documents/notes/home.org" "Inbox")
         "* TODO %?\n  %i")
        ("th" "Homework" entry (file+olp "~/Documents/notes/home.org" "Inbox")
         "* TODO %?\n  %i")))

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
  (org-roam-setup))

(use-package visual-fill-column
  :hook (org-mode . org-mode-visual-fill))

(defun org-mode-visual-fill ()
  (setq visual-fill-column-width 100
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package org-appear)

;; Hide org markup
(setq-default org-hide-emphasis-markers t)
(add-hook 'org-mode-hook 'org-appear-mode)

(require 'org-eldoc)
(add-hook 'org-mode-hook 'org-eldoc-load)
(setq org-eldoc-breadcrumb-separator " 󰍟 ")

(use-package org-modern
  :hook (org-mode . org-modern-mode)
  :config
  (setq
   ;; org-modern-star '("＊" "  ＊" "    ＊" "      ＊")
   org-modern-star '( "" "  " "    " "      ")
   org-modern-list '((42 . "◦") (43 . "•") (45 . "–"))
   org-modern-block-name nil
   org-modern-keyword nil
   org-modern-todo t
   org-modern-table nil)

  (set-face-attribute 'org-modern-done nil :foreground dim-fgcolor :background bgcolor :weight 'bold :height 130 :inherit 'nil)
  (set-face-attribute 'org-modern-time-inactive nil :foreground dim-fgcolor :background darker-bgcolor :height 130 :inherit 'nil)
  (set-face-attribute 'org-modern-time-inactive nil :foreground dim-fgcolor :background grim-bgcolor :height 130 :inherit 'nil)
  (set-face-attribute 'org-modern-time-active nil :background dim-fgcolor :foreground darker-bgcolor :height 130 :inherit 'nil)
  )

(use-package org-gcal
  :defer t
  :config
  (setq org-gcal-client-id "491318009852-srusa34d7j9gnocfkrvjrt7ej686mj0c.apps.googleusercontent.com"
        org-gcal-client-secret "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"
        org-gcal-fetch-file-alist '(("chillyashrada@gmail.com" .  "~/Documents/schedule.org")))
  )

(use-package evil-org
  :diminish evil-org-mode
  :after org
  :config
  (add-hook 'org-mode-hook 'evil-org-mode)
  (add-hook 'evil-org-mode-hook
            (lambda () (evil-org-set-key-theme))))

(require 'evil-org-agenda)
(evil-org-agenda-set-keys)

(setq org-agenda-files '("~/Documents/notes/home.org")
      org-agenda-breadcrumbs-separator " ❱ "
      org-directory "~/Documents/")
(setq org-agenda-hidden-separator "‌‌ ")
(setq org-agenda-block-separator (string-to-char " "))
(setq org-agenda-format-date 'my-org-agenda-format-date-aligned)
(setq org-agenda-block-separator nil)

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

;; How to open buffer when calling `org-edit-special'.
(setq org-src-window-setup 'current-window)

(defun e/org-babel-edit ()
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

(add-hook 'org-mode-hook 'org-eldoc-load)
(setq org-ellipsis "  ")
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
(add-hook 'org-mode-hook (lambda () (variable-pitch-mode 1)))
