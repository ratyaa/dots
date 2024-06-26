(defun my/try-xdg-open (filename)
  "Try to open file with xdg-open and return current buffer,
  otherwise apply `find-file-noselect'."
  (if (not (member (file-name-extension filename) my/xdg-open-extensions))
      (find-file-noselect filename)
    (browse-url-xdg-open (expand-file-name filename))
    t))

(defun my/find-file-noselect-try-xdg (filename &optional wildcards)
  "If FILENAME is a directory, apply `find-file-noselect', otherwise
  expand wildcards and recursively apply `my/try-xdg-open'."
  (setq filename
	(abbreviate-file-name
	 (expand-file-name filename)))
  (if (file-directory-p filename)
      (find-file-noselect filename)
    (if (and wildcards
	     find-file-wildcards
	     (not (file-name-quoted-p filename))
	     (string-match "[[*?]" filename))
	(let ((files (condition-case nil
			 (file-expand-wildcards filename t)
		       (error (list filename))))
	      (find-file-wildcards nil))
	  (if (null files)
	      (my/try-xdg-open filename)
	    (mapcar #'my/try-xdg-open files)))
      (my/try-xdg-open filename))))

(defun my/find-file-try-xdg (filename &optional wildcards)
  "The same as `find-file', except than if found FILENAMEs have an
  extension that is a member of `my/xdg-open-extensions' list, open
  them via xdg-open."
  (interactive
   (find-file-read-args "Find file: "
			(confirm-nonexistent-file-or-buffer)))
  (let ((value (my/find-file-noselect-try-xdg filename wildcards)))
    (cond ((eq value t) nil)
	  ((listp value) (mapcar 'pop-to-buffer-same-window (nreverse (delq t value))))
	  (t (pop-to-buffer-same-window value)))))
(defun my/emacs-popup-find-file ()
  "Call `find-file' in a pop-up minibuffer-only frame."
  (call-process "bspc" nil nil nil "rule" "-a" "Emacs" "rectangle=")
  (call-process "bspc" nil nil nil "rule" "-a" "Emacs" "state=floating")
  (make-frame
   '((name . "<file-launcher>")
     (minibuffer . only)
     (width . 60)
     (height . 10)))
  (call-interactively #'my/find-file-try-xdg)
  (delete-frame (select-frame-by-name "<file-launcher>"))
  (call-process "bspc" nil nil nil "rule" "-a" "Emacs" "state=tiled"))
