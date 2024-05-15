;;(with-current-buffer "*Messages*"
;;  (messages-buffer-mode)
;;  (message "Snake init!"))

(print "Snake init!")
(defun hcv-trace-load (filename &optional noerror nomessage nosuffix must_suffix)
;;  (princ (concat "snake" file)))
  (message (concat "snake " file)))
;;  (append-to-file (concat "loading " filename) nil "~/snake.txt"))

(advice-add 'load :before #'hcv-trace-load)
;; (debug-on-entry 'hcv-trace-load)
(setq force-load-messages t)
(setq top-level '(normal-top-level))
(normal-top-level)
