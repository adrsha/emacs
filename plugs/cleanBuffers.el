;; GET RID OF THE EXTRA BUFFERS

;; Removes *scratch* from buffer after the mode has been set.
;; (defun remove-scratch-buffer ()
;;   (if (get-buffer "*scratch*")
;;       (kill-buffer "*scratch*")))
;; (add-hook 'after-change-major-mode-hook 'remove-scratch-buffer)

;; DIRECTLY
;; Removes *messages* from the buffer.
(kill-buffer "*Messages*")

;; WITH A HOOK
;; Removes *Completions* from buffer after you've opened a file.
(add-hook 'minibuffer-exit-hook
          (lambda ()
             (let ((buffer "*Completions*"))
               (and (get-buffer buffer)
                    (kill-buffer buffer)))))

(add-hook 'minibuffer-exit-hook
          (lambda ()
             (let ((buffer "*Async-native-compile-log"))
               (and (get-buffer buffer)
                    (kill-buffer buffer)))))

;; WITH REGEX
;; Prevent getting into the following buffers by previous buffer or next buffer cmds.
(defcustom buffer-skip-regexp
  (rx bos (or (or "*Backtrace*" "*Compile-Log*" "*Completions*"
                  "*Messages*" "*package*" "*Warnings*"  )
              (seq "magit-diff" (zero-or-more anything))
              (seq "magit-process" (zero-or-more anything))
              (seq "magit-revision" (zero-or-more anything))
              (seq "magit-stash" (zero-or-more anything)))
      eos)
  "Regular expression matching buffers ignored by `next-buffer' and
`previous-buffer'."
  :type 'regexp)

(defun buffer-skip-p (window buffer bury-or-kill)
  "Return t if BUFFER name matches `aj8/buffer-skip-regexp'."
  (string-match-p buffer-skip-regexp (buffer-name buffer)))

(setq switch-to-prev-buffer-skip 'buffer-skip-p)

(provide 'cleanBuffers)
