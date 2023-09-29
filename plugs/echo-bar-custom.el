(qv/package echo-bar)

(echo-bar-enable)

(setq echo-bar-function 'qv/echo-bar-function)
(setq echo-bar-update-interval 1)
(setq echo-bar-right-padding 4)
(setq qv/echo-bar-height 1.2)

(qv/face qv/icons :family "all-the-icons")

(defface eaTime-icon
  '((t :inherit org-tag))
  "Echo Area Time face."
  :group 'ea-faces)

(defface eaBattery-charge-icon
  '((t :inherit org-imminent-deadline))
  "Echo Area Battery face."
  :group 'ea-faces)

(defface eaBattery-icon
  '((t :inherit org-tag))
  "Echo Area Battery face."
  :group 'ea-faces)

(defface eaDate-icon
  '((t :inherit org-tag))
  "Echo Area Date face."
  :group 'ea-faces)

(defface eaTime
  '((t :inherit org-date))
  "Echo Area Time face."
  :group 'ea-faces)

(defface eaBattery
  '((t :inherit org-date))
  "Echo Area Battery face."
  :group 'ea-faces)

(defface eaDate
  '((t :inherit org-date))
  "Echo Area Date face."
  :group 'ea-faces)

(defface eaBuf
  '((t :inherit org-date))
  "Echo Area Date face."
  :group 'ea-faces)

(defface eaLinePos
  '((t :inherit org-tag))
  "Echo Area Date face."
  :group 'ea-faces)

(defface eaSep
  '((t :inherit shadow))
  "Echo Area separator face."
  :group 'ea-faces)


(defun qv/echo-bar-function ()
  ;; (format "%s%s %s%s%s %s %s%s"
  (format "%s%s %s%s%s"
          (propertize " " 'display `(height ,qv/echo-bar-height))
          (or (ignore-errors (qv/activity-string)) "")
          (or (ignore-errors (qv/battery-format)) "")
          ;;   (propertize
          ;; "󰃭 "
          ;;    'face 'eaDate-icon) 

          ;;   (propertize
          ;; (format-time-string "%b %d")
          ;;    'face 'eaDate) 

          ;; (propertize "󰇙" 'face 'eaSep)

            (propertize
          "  "
             'face 'eaTime-icon) 
            (propertize
          (format-time-string "%I:%M")
            'face 'eaTime)
  ))

(require 'battery)

(defun qv/battery-format ()
  (when-let* ((func battery-status-function)
              (status (funcall func))
              (percent (round (string-to-number (battery-format "%p" status))))
              (power-method (battery-format "%L" status)))
    (format "%s %s %s%s%s %s%s %s %s %s "

            (propertize
            (format "%s" (if (and (buffer-file-name) (buffer-modified-p)) "❋" ""))
            'face 'eaSep) 

            (propertize
             (if (buffer-file-name)
                 (file-name-nondirectory (buffer-file-name))
               (buffer-name))
            'face 'eaBuf) 

            (propertize 
            (format " %s" (line-number-at-pos))
            'face 'eaLinePos)

            (propertize "" 'face 'eaSep)
            
            (propertize 
             (format "%s" (count-lines (point-min) (point-max)))
             'face 'eaLinePos)
            (propertize "󰇙" 'face 'eaSep)

            (propertize
             (if (string= power-method "AC") "  " "")
             'face 'eaBattery-charge-icon) 
            (propertize
            (cond ((>= percent 95) "󰁹")
                  ((>= percent 70) "󰂀")
                  ((>= percent 50) "󰁾")
                  ((>= percent 15) "󰁺")
                  (t "󰂎"))
             'face 'eaBattery-icon) 
            (propertize
            (concat (number-to-string percent) "%" )
             'face 'eaBattery) 
            (propertize "󰇙" 'face 'eaSep)
            )))
(provide 'echo-bar-custom)
