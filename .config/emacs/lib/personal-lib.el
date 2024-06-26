;;; -*- lexical-binding: t -*-
(defun emacs-report-init-timestamp (mesg)
     (let* ((time (current-time))
	    (str (format "%f seconds"
			(float-time (time-subtract time
						   current-init-timestamp)))))
       (setq current-init-timestamp time)
       (message "%s" (concat str " " mesg))))

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
  (interactive)
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

(defun my/cdlatex-tab-backwards ()
  "`cdlatex-tab', but for backward movement"
  (interactive)
  (catch 'stop
    (while (re-search-backward "[ )}\n]\\|\\]" (point-min) t)
      (backward-char 1)
      (cond
       ((= (following-char) ?\ )
	;; stop at first space or b-o-l
	(if (not (bolp)) (forward-char 1)) (throw 'stop t))
       ((= (following-char) ?\n)
	;; stop at line end, but not after \\
	(if (and (bolp) (not (eobp)))
	    (throw 'stop t)
	  (if (equal "\\\\" (buffer-substring-no-properties
			     (- (point) 2) (point)))
	      (forward-char 1)
	    (throw 'stop t))))
       (t
	;; Stop before )}] if preceding-char is any parenthesis
	(if (or (= (char-syntax (preceding-char)) ?\()
		(= (char-syntax (preceding-char)) ?\))
		(= (preceding-char) ?-))
	    (throw 'stop t)
	  (forward-char 1)
	  (if (looking-at "[^_^({\\[]")
	      ;; stop after closing bracket, unless ^_[{( follow
	      (throw 'stop t))))))))

(declare-function yas-expand nil)
(defun my/yas-try-expanding-auto-snippets ()
  (when (and (boundp 'yas-minor-mode) yas-minor-mode)
    (let ((yas-buffer-local-condition ''(require-snippet-condition . auto)))
      (yas-expand))))

(defun my/tangle-config ()
  "Tangle and compile my config file."
  (interactive)
  (when (buffer-file-name)
    (let ((prog-mode-hook nil)
	  (orgfile (file-truename (buffer-file-name))))
      (when (equal orgfile (file-truename (concat user-emacs-directory my/config)))
	(let* ((prog-mode-hook nil)
	       (targets (org-babel-tangle-file orgfile)))
	  (dolist (target targets)
	    (byte-compile-file target)
	    (native-compile target)
	  targets))))))

(declare-function cdlatex-number-of-backslashes-is-odd nil)
(declare-function cdlatex--texmathp nil)
(declare-function cdlatex-ensure-math nil)

(defun my/sub-superscript (sub-superscript-char)
  "Insert ^{} or _{} unless the number of backslashes before point is odd.
  When not in LaTeX math environment, _{} and ^{} will have dollars.
  When pressed twice, make the sub/superscript roman."
  (interactive)
  (if (and cdlatex-make-sub-superscript-roman-if-pressed-twice
	   (equal this-command last-command))
      (progn
	(insert "\\mathrm{}")
	(backward-char 1))
    (if (cdlatex-number-of-backslashes-is-odd)
	;; Quoted
	(insert sub-superscript-char)
      ;; Check if we are in math mode, if not switch to or only add _ or ^
      (if (not (or (cdlatex--texmathp)
		   cdlatex-sub-super-scripts-outside-math-mode))
	  (insert sub-superscript-char)
	(cdlatex-ensure-math)
	;; Insert the normal template.
	(insert sub-superscript-char)
	(insert "{}")
	(forward-char -1)))))

(defun my/latex-insert-alternate (text math)
  "Eval s-exp dependently on `texmathp' value."
  (if (texmathp)
      (eval math t)
    (eval text t)))

(defun my/config-display-line-numbers-faces ()
  "My faces configuration for `display-line-numbers-mode' minor mode."
  (dolist (face my/display-line-numbers-faces)
      (apply #'set-face-attribute face)))

(defun my/config-font-lock-faces ()
  "My faces configuration for `font-lock-mode' minor mode."
  (dolist (face my/font-lock-faces)
    (apply #'set-face-attribute face)))

(defun my/config-org-faces ()
  "My faces configuration for org-mode."
  (dolist (face my/org-faces)
    (apply #'set-face-attribute face)))

(defun my/config-emacs-faces ()
  "My configuration for the built-in emacs faces."
  (dolist (face my/emacs-faces)
    (apply #'set-face-attribute face)))

(defun my/update-faces ()
  "Load personal faces config."
  (dolist (update-function my/update-feature-face-functions)
    (when (featurep (car update-function))
      (funcall (cdr update-function)))))

(defun my/toggle-themes ()
  "Call `modus-themes-toggle' and then `my/update-faces'."
  (interactive)
  (modus-themes-toggle)
  (my/update-faces))

(defconst my/config "init.org"
  "Name of my emacs literate .org config file.")

(defvar my/emacs-faces
  '((default nil
	     :font "JetBrainsMono Nerd Font Mono"
	     :height 100)
    (fixed-pitch nil
		 :font "JetBrainsMono Nerd Font Mono"
		 :height 100)
    (variable-pitch nil
		    :font "Merriweather"
		    :height 105
		    :weight regular
		    :width ultra-expanded)))

(defvar my/org-faces
  '((org-default nil
		 :inherit 'variable-pitch)
    (org-block nil
	       :inherit 'fixed-pitch)
    (org-block-begin-line nil
			  :inherit 'font-lock-comment-face)
    (org-block-end-line nil
			:inherit 'org-block-begin-line)))

(defvar my/display-line-numbers-faces
  '((line-number nil
		 :inherit 'line-number
		 :font "JetBrainsMono Nerd Font Mono"
		 :height 100)))

(defvar my/font-lock-faces
  '((font-lock-comment-face nil
			    :inherit 'font-lock-comment-face
			    :font "JetBrainsMono Nerd Font Mono"
			    :height 100
			    :slant italic)))

(defvar my/update-feature-face-functions
  '((faces . my/config-emacs-faces)
    (font-core . my/config-font-lock-faces)
    (display-line-numbers . my/config-display-line-numbers-faces)
    (org . my/config-org-faces)))
