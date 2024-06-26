(defun emacs-report-init-timestamp (mesg)
    (let* ((time (current-time))
	   (str (format "%f seconds"
		       (float-time (time-subtract time
						  current-init-timestamp)))))
      (setq current-init-timestamp time)
      (message "%s" (concat str " " mesg))))
