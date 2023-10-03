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

(provide 'cleanBuffers)
