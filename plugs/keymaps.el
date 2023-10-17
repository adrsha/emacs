;;-*- lexical-binding: t; -*-

;; ░██████╗░███████╗███╗░░██╗███████╗██████╗░░█████╗░██╗░░░░░
;; ██╔════╝░██╔════╝████╗░██║██╔════╝██╔══██╗██╔══██╗██║░░░░░
;; ██║░░██╗░█████╗░░██╔██╗██║█████╗░░██████╔╝███████║██║░░░░░
;; ██║░░╚██╗██╔══╝░░██║╚████║██╔══╝░░██╔══██╗██╔══██║██║░░░░░
;; ╚██████╔╝███████╗██║░╚███║███████╗██║░░██║██║░░██║███████╗
;; ░╚═════╝░╚══════╝╚═╝░░╚══╝╚══════╝╚═╝░░╚═╝╚═╝░░╚═╝╚══════╝

;;
;; KEYMAPS
;;

;; Custom Functions
(defcustom my-skippable-buffers '("*Warnings*" "*Messages*" "*scratch*" "*Help*" "*Completions*" "*flycheck-posframe-buffer" "*Async-native-compile-log*")
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

(global-set-key [remap next-buffer] 'my-next-buffer)
(global-set-key [remap previous-buffer] 'my-previous-buffer)

(general-def
  "C-j" 'nil
  "C-k" 'nil)

;; (define-key vertico-map (kbd "TAB") 'vertico-next)
;; (keymap-set vertico-map "C-l" '(lambda () (interactive) (vertico-insert) (minibuffer-force-complete-and-exit)))
;; Using RETURN to follow links in Org/Evil
;; Unmap keys in 'evil-maps if not done, (setq org-return-follows-link t) will not work
;; GNRL
;; LEADER KEY

(general-create-definer e/leader-keys
  :keymaps '(normal insert visual emacs)
  :prefix "SPC"
  :global-prefix "C-SPC"
  )

(e/leader-keys
  "SPC" '(execute-extended-command :which-key "  M-x")
  "k"  '(eldoc-box-help-at-point :which-key "󰆽 hover")
  ;;LSP
  "c"  '(:ignore t :which-key "󰅱 code")
  "cr"  '(eglot-rename :which-key " rename symbol")
  "ce"  '(org-ctrl-c-ctrl-c :which-key " execute code in org")
  "cc"  '(format-all-buffer :which-key " format region or buffer")
  "cl"  '(crux-cleanup-buffer-or-region :which-key " cleanup")
  "cf" '((lambda () (interactive) (indent-region (point-min) (point-max))) :wk "  enable")
  "cd"  '(aphelia-format-buffer :which-key "󰉼 format"))

(e/leader-keys
  "f"  '(:ignore t :which-key "󰈔 files")
  "ff" '(find-file :which-key "󰈞 find a file")
  "fr" '(consult-recent-file :which-key "󰣜 recent files")
  "fn" '(org-roam-node-find :which-key "󰣜 find nodes")
  "fc"  '(:ignore t :which-key "󰈔 current file")
  "fcs" '(crux-sudo-edit :which-key "󱄛 open this file as sudo")
  "fcd" '(crux-delete-buffer-and-file :which-key "󱄛 open this file as sudo")
  "fcr" '(crux-rename-buffer-and-file :which-key "󰑕 rename this file"))

(e/leader-keys
  "o"  '(:ignore t :which-key "󰉋 open")
  "ow" '(crux-open-with :which-key " open with")
  "od" '(dired-jump :which-key "󰉓  open dired")
  "oh" '(hydra-hydras/body :which-key "󰊠 open hydras")
  "oe" '(e/org-babel-edit :which-key "󰕪  open agendas")
  "oa" '(org-agenda :which-key "󰕪  open agendas")
  "oc" '(org-capture :which-key "󰄄  open capture")
  "or" '(consult-yank-from-kill-ring :which-key " open registry and yank")
  "og"  '(org-roam-graph :which-key "Open graph")
  "oF" '(list-faces-display :which-key " open with"))


(e/leader-keys
  "i" '(:ignore t :which-key "insert")
  "is" '(org-schedule :which-key "󰾖  insert schedule")
  "id" '(org-deadline :which-key "󰾕  insert deadline")
  "il" '(org-insert-link :which-key "  insert link")
  "in" '(org-roam-node-insert :which-key "  insert link")
  "ic" '(:ignore t :which-key " insert cursor")
  "icI" '(evil-mc-make-cursor-in-visual-selection-beg :which-key " insert cursor at the beginning")
  "icA" '(evil-mc-make-cursor-in-visual-selection-end :which-key " insert cursor at the end")
  "icc" '(hydra-insert-cursor/body :which-key " insert cursor hydra")
  ;; "C-c n c"  org-roam-capture
  ;; Dailies
  ;; "C-c n j"  org-roam-dailies-capture-today
  "it" '(org-insert-time-stamp :which-key "  insert time stamp   ")
  )

(e/leader-keys
  "b"  '(:ignore t :which-key " buffer navigation")
  "bd" '(kill-buffer-and-window :which-key "󰆴 kill the current buffer and window")
  "bk" '(kill-some-buffers :which-key "󰛌 kill all other buffers and windows")
  "bn" '(next-buffer :which-key "󰛂 switch buffer")
  "bp" '(previous-buffer :which-key "󰛁 switch buffer")
  "bb" '(consult-buffer :which-key "󰕰 view buffers"))


(e/leader-keys
  "s"  '(:ignore t :which-key " search")
  "ss" '(consult-line :which-key "󰱼 line search")
  "si" '(nerd-icons-insert :which-key "󰭟 search for icons")
  "srg" '(consult-ripgrep :which-key "󰟥 search with rg")
  "sd" '(dictionary-search :which-key "󰬋 search in dictionary"))

(e/leader-keys
  "e"  '(:ignore t :which-key "󰈈  evaluate")
  "eb" '(eval-buffer :which-key "󰷊  evaluate buffer")
  "ee" '(eval-last-sexp :which-key "󰷊  evaluate last expression")
  "er" '(eval-region :which-key "󰨺  evaluate region"))

(e/leader-keys
  "p"  '(:ignore t :which-key "󰅱 project")
  "pr"  '(projectile-recentf :which-key "󰈞 recentf")
  "pv"  '(:ignore t :which-key "󰅱 view")
  "pvc"  '(projectile-vc :which-key "󰈞 view changes")
  "pvd"  '(projectile-browse-dirty-projects :which-key "󰈞 view dirty projects")
  "ps"  '(:ignore t :which-key "󰅱 switch")
  "psp"  '(projectile-switch-open-project :which-key "󰅱 switch project"))

(e/leader-keys
  "h"  '(:ignore t :which-key "󰞋 help")
  "ht" '(helpful-at-point :which-key " describe this")
  "hF" '(describe-face :which-key "󱗎 describe face")
  "hf" '(helpful-function :which-key "󰯻 describe function")
  "hb" '(embark-bindings :which-key "󰌌 describe bindings")
  "hk" '(helpful-key :which-key "󰯻 describe this key")
  "hv" '(helpful-variable :which-key " describe variable")
  "hr" '(:ignore t :which-key "󱍸 reload")
  "hrb" '(revert-buffer-quick :which-key "󰄸 reload buffer")
  "hrr" '((lambda () (interactive) (load-file "~/.config/emacs/init.el")) :wk "  Reload emacs config"))

(e/leader-keys
  "t"  '(:ignore t :which-key "  toggles/switches")
  "tt"  '(toggle-truncate-lines :which-key "󰖶 toggle word wrap mode")
  "tc"  '(:ignore t :which-key "󰮫 toggle completion")
  "tce" '((lambda () (interactive) (setq-default corfu-auto t) (corfu-mode 1)) :wk "  enable")
  "tcd" '((lambda () (interactive) (setq-default corfu-auto nil) (corfu-mode 1)) :wk "  disable")
  "tr"  '(org-roam-buffer-toggle :which-key "Roam Buffer")
  "tm"  '(minimap-mode :which-key "󰍍 minimap toggles"))


(general-create-definer e/goto-keys
  :keymaps '(normal insert)
  :prefix "g"
  :global-prefix "C-g"
  )

(e/goto-keys
  "n"  '(flycheck-next-error :which-key " next error")
  "p"  '(flycheck-previous-error :which-key " next error")
  )

(general-def
  "M-p" 'popper-toggle-type
  "M-n" 'popper-cycle
  "M-S-n" 'popper-cycle-backwards
  "M-d" 'popper-kill-latest-popup
  "C-;" 'embark-become
  "C-<return>" 'embark-act
  "K" 'nil
  "<escape>" 'keyboard-escape-quit)

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


(provide 'keymaps)
;;; keymaps.el ends here
