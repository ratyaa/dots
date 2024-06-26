(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/") t)

;;; -*- lexical-binding: t -*-
(autoload #'server-running-p "server")
(unless (server-running-p)
  (server-start))

(setq use-dialog-box nil)


(savehist-mode)

(recentf-mode)

(global-auto-revert-mode)

(defvar my/xdg-open-extensions
  (list "pdf" "mkv")
  "List of filename extensions which need to be opened via xdg-open.")

(defconst dotfiles-user-emacs-dir
  (concat (getenv "HOME") "/.config/dotfiles/.config/emacs/"))

(defvar personal-keybindings nil)

(keymap-global-set "C-x C-f" #'my/find-file-try-xdg)
(keymap-global-set "C-+" #'text-scale-increase)
(keymap-global-set "C--" #'text-scale-decrease)
(keymap-global-set "C-c l" #'org-store-link)
(keymap-global-set "C-c a" #'org-agenda)
(keymap-global-set "C-c c" #'org-capture)
(keymap-global-unset "C-z" t)
(keymap-global-set "M-z" #'zap-to-char)
(keymap-global-set "C-c n" #'notmuch)
(keymap-global-set "C-S-h" 'help-command)
(keymap-global-set "C-h" #'delete-backward-char)

(add-hook 'font-lock-mode-hook #'my/config-font-lock-faces)
(add-hook 'display-line-numbers-mode-hook #'my/config-display-line-numbers-faces)

;; (set-frame-parameter nil 'alpha-background 85)
;; (add-to-list 'default-frame-alist '(alpha-bakground . 85))

(use-package emacs
  :init
  (setq modus-themes-bold-constructs t
	modus-themes-italic-constructs t
	modus-themes-syntax '(alt-syntax)
	modus-themes-mode-line '(borderless accented)
	;; modus-themes-fringes nil
	modus-themes-hl-line '(accented intense)
	modus-themes-subtle-line-numbers t
	modus-themes-paren-match '(bold)
	modus-themes-region '(accented bg-only))

  (set-fontset-font t 'han "Noto Sans CJK SC")
  (set-fontset-font t 'han "Noto Sans CJK TC" nil 'append)
  (set-fontset-font t 'symbol (font-spec :family "Noto Color Emoji") nil 'prepend) 
  :config
  (load-theme 'modus-vivendi)
  (my/update-faces))

(use-package olivetti
  :defer t
  :functions elfeed-show-refresh
  :after elfeed
  :config
  (defun my/elfeed-show (buffer)
    (switch-to-buffer buffer)
    (olivetti-mode)
    (elfeed-show-refresh)))

(use-package yasnippet
  :defer t
  :commands yas-minor-mode
  :config
  (setq yas-snippet-dirs '("~/.snippets"))
  (setq yas-triggers-in-field t)

  (declare-function yas-reload-all nil)
  (yas-reload-all)
  (add-hook 'LaTeX-mode-hook 'yas-minor-mode))

(use-package tex
  :defer t
  :ensure auctex
  ;; :defines tex--prettify-symbols-alist
  :config
  (require 'org)
  (declare-function TeX-revert-document-buffer nil)
  (declare-function TeX-source-correlate-mode nil)

  (defun my/latex-setup ()
    "My configuration to LaTeX-mode"
    (setq TeX-command-extra-options "--synctex=1")

    ;; Modes
    (make-local-variable 'input-method-activate-hook)
    (add-hook 'input-method-activate-hook 'my/latex-cache-im nil t)
    (add-hook 'input-method-deactivate-hook 'my/latex-cache-im-deactivated nil t)
    (add-hook 'post-command-hook 'my/latex-im-autoswitch nil t)
    (add-hook 'post-self-insert-hook 'my/yas-try-expanding-auto-snippets nil t)

    (TeX-fold-mode t)
    (TeX-source-correlate-mode t)
    (display-line-numbers-mode t)

    (let ((my/tex-prettify-symbols-alist
	   '(( "\\left( " . ?\( )
	     ( " \\right)" . ?\) )
	     ( "\\left[ " . ?\[ )
	     ( " \\right]" . ?\] )
	     ( "\\left\\{ " . ?\{ )
	     ( " \\right\\}" . ?\} )
	     ( "\\left| " . ?\| )
	     ( " \\right|" . ?\| )
	     ( "\\left< " . ?\< )
	     ( " \\right>" . ?\> )
	     ( "\\mathrm{d}" . ?ꟈ)
	     ( "\\frac" . ?÷)
	     ( "\\limits" . ?|)
	     ( "\\left." . ?< )
	     ( "\\iint" . ?∬ )
	     ( "\\iiint" . ?∭ ))))
      (dolist (my/pretty my/tex-prettify-symbols-alist)
	(add-to-list 'prettify-symbols-alist my/pretty)))
    (delete '("--" . 8211) prettify-symbols-alist)
    (prettify-symbols-mode t))
  (add-hook 'LaTeX-mode-hook 'my/latex-setup)
  (add-to-list 'TeX-view-program-list
	       '("my-pdf-viewer"
		 ("zathura-float %o"
		  ;; (mode-io-correlate
		  ;; " --synctex-forward %n:0:\"%(project-dir)tex/main.tex\" -x \"emacsclient +%{line} %{input}\""))
		  (mode-io-correlate " --synctex-forward %n:0:\"%b\" -x \"emacsclient +%{line} %{input}\""))
		 "zathura-float"))

  (add-to-list 'TeX-view-program-selection '(output-pdf "my-pdf-viewer"))

  (add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer)

  ;; (setq TeX-electric-math '("\\(" . "\\)"))
  (setq TeX-source-correlate-start-server t)
  (setq auto-mode-alist (cons '("\\.tex$" . latex-mode) auto-mode-alist))
  (setq TeX-parse-self t)
  (setq japanese-TeX-error-messages nil)



  (use-package cdlatex
    :ensure t
    :hook (LaTeX-mode . turn-on-cdlatex)
    ;; :config
    ;; (setopt cdlatex-math-modify-prefix "C-S-z")
    ;; (setopt cdlatex-math-symbol-prefix "C-z")
    :bind (:map
	   cdlatex-mode-map
	   ("<tab>" . cdlatex-tab)
	   ("<backtab>" . my/cdlatex-tab-backwards)
	   ("C-z" . cdlatex-math-symbol)
	   ("C-S-z" . cdlatex-math-modify)
	   :map orgtbl-mode-map
	   ("<tab>" . lazytab-org-table-next-field-maybe))
    :config
    (load-file (concat user-emacs-directory "lazytab.el"))
    (require 'lazytab)
    (lazytab-mode t)

    (setq cdlatex-command-alist
	  '(
	    ("pd"		"Insert a partial derivative frac"
	     "\\frac{\\partial ?}{\\partial }" cdlatex-position-cursor nil nil t)
	    ("ppd"		"Insert a second order partial derivative frac (one var)"
	     "\\frac{\\partial^2 ?}{\\partial ^2}" cdlatex-position-cursor nil nil t)
	    ("ddd"		"Insert a second order full derivative frac (one var)"
	     "\\frac{\\mathrm{d}^2 ?}{\\mathrm{d} ^2}" cdlatex-position-cursor nil nil t)
	    ("pdpd"		"Insert a second order partial derivative frac (two vars)"
	     "\\frac{\\partial^2 ?}{\\partial \\partial }" cdlatex-position-cursor nil nil t)
	    ("dd"		"Insert a full derivative frac"
	     "\\frac{\\mathrm{d}?}{\\mathrm{d} }" cdlatex-position-cursor nil nil t)
	    ("fr"		"Insert \\frac{}{}"
	     "\\frac{ ? }{ }" cdlatex-position-cursor nil nil t)
	    ("equ*"		"Insert an EQUATION* environment template"
	     "" cdlatex-environment ("equation*") t nil)
	    ("ss*"		"Insert a \\subsection*{} statement"
	     "\\subsection*{?}" cdlatex-position-cursor nil t nil)
	    ("sss*"		"Insert a \\subsubsection*{} statement"
	     "\\subsubsection*{?}" cdlatex-position-cursor nil t nil)
	    ("отв"		"Insert my standard answer statement"
	     "\\paragraph{\\textbf{Ответ:}} $\\displaystyle ?$." cdlatex-position-cursor nil t nil)
	    ("seq"	"Insert a system of linear equations"
	     "\\begin{dcases}
      ?
      \\end{dcases}" cdlatex-position-cursor nil nil t)
	    ("ceq"		"Insert a collection of linear equations"
	     "\\begin{sqdcases}
      ?
      \\end{sqdcases}" cdlatex-position-cursor nil nil t)
	    ("prodl"		"Insert \\prod\\limits_{}^{}"
	     "\\prod\\limits_{?}^{}" cdlatex-position-cursor nil nil t)
	    ("iintl"		"Insert \\iint\\limits_{}^{}"
	     "\\iint\\limits_{?}^{}" cdlatex-position-cursor nil nil t)
	    ("iiintl"		"Insert \\iiint\\limits_{}^{}"
	     "\\iiint\\limits_{?}^{}" cdlatex-position-cursor nil nil t)
	    ("pmx"		"Insert pmatrix env"
	     "\\begin{pmatrix} ? \\end{pmatrix}" lazytab-position-cursor-and-edit nil nil t)))

    (setq cdlatex-math-symbol-alist
	  '(
	    ( ?\[ ("\\Leftarrow"      "\\Longleftarrow"	"\\hookleftarrow"))
	    ( ?\] ("\\Rightarrow"     "\\Longrightarrow"	"\\hookrightarrow"))))

    (setq cdlatex-math-modify-alist
	  '(
	    ( ?\] "\\text{?}" "$?$" nil nil nil)
	    ( ?l "\\left. ? \\right|" nil nil nil nil)))

    (define-key cdlatex-mode-map "/"
		#'(lambda ()
		    (interactive)
		    (my/latex-insert-alternate '(insert "/") '(my/sub-superscript "^"))))
    (define-key cdlatex-mode-map "."
		#'(lambda ()
		    (interactive)
		    (my/latex-insert-alternate '(insert ".") '(my/sub-superscript "_"))))
    (define-key cdlatex-mode-map "_"
		#'(lambda ()
		    (interactive)
		    (my/latex-insert-alternate '(insert "_") '(insert "."))))
    (define-key cdlatex-mode-map "^"
		#'(lambda ()
		    (interactive)
		    (my/latex-insert-alternate '(insert "^") '(insert "/"))))
    (keymap-set cdlatex-mode-map "C-`" #'prettify-symbols-mode))

  (defun my/latex-cache-im ()
    "Set (whether the point is in math mode) variables `my/latex-current-math-im'
	or `my/latex-current-text-im' as freshly switched im"
    (when (eq major-mode 'latex-mode)
      (if (texmathp)
	  (setq my/latex-current-math-im current-input-method)
	(setq my/latex-current-text-im current-input-method))))

  (defun my/latex-cache-im-deactivated ()
    "Set (whether the point is in math mode) variables `my/latex-current-math-im'
	or `my/latex-current-text-im' as nil"
    (when (eq major-mode 'latex-mode)
      (if (texmathp)
	  (setq my/latex-current-math-im nil)
	(setq my/latex-current-text-im nil))))

  (defun my/latex-im-autoswitch ()
    "Switch input method automatically based on whether is the point in math mode."
    (when (eq major-mode 'latex-mode)
      (if (texmathp)
	  (unless (string= current-input-method my/latex-current-math-im)
	    (activate-input-method my/latex-current-math-im))
	(unless (string= current-input-method my/latex-current-text-im)
	  (activate-input-method my/latex-current-text-im))))))

(defvar-local my/latex-current-text-im nil
  "Last im used in nonmath-mode in a LaTeX buffer")

(defvar-local my/latex-current-math-im nil
  "Last im used in math-mode in a LaTeX buffer")

;; (defvar rattex--active-keymap 'rattex-mode-map)

;; (defvar rattex-track-size 10
;;   "Amount of tracked last inserted racks.")

;; (defvar rattex-derivative-keymap
;;   (let ((map (make-sparse-keymap)))
;;     (define-key map "M-d" #'rattex-insert-partial-derivative)
;;     map)
;;   "rattex keymap")

;; (defun rattex--insert-derivative (d-string)
;;   "Insert derivative with `d' as given string."
;;   (let ((start-point (point))
;; 	(end-point (point))
;; 	(d-length (length d-string)))
;;     (insert "\\frac{}{}")
;;     (backward-char 3)
;;     (insert (concat d-string " "))
;;     (forward-char 2)
;;     (insert (concat d-string " "))
;;     (backward-char (+ 3 d-length))
;;     (list 'start-point start-point
;; 	  'end-point end-point
;; 	  'rattex-rack-type 'rattex--partial)))

;; (defvar rattex-mode-map rattex-derivative-keymap
;;   "Keymap for rattex-mode")

;; (defun rattex-insert-partial-derivative ()
;;   "Insert a partial derivative."
;;   (interactive)
;;   (rattex--insert-derivative "\\partial"))

;; (defvar rattex--snippet-auto-p nil
;;   "Current state of autosnippet functionality.")

;; (defun rattex-snippet-toggle-auto ()
;;   "Toggle autosnippet functionality."
;;   (interactive)
;;   (if rattex--snippet-auto-p
;;       (remove-hook 'post-self-insert-hook 'rattex-snippet-try-expand)
;;     (add-hook 'post-self-insert-hook 'rattex-snippet-try-expand)))

;; (defvar rattex--snippet-table
;;   (make-hash-table
;;    :test 'equal
;;    :weakness nil
;;    :size 20
;;    :rehash-size 20
;;    :rehash-threshold 0.5))

;; (defun rattex-snippet-set-table (snippets-alist)
;;   "Create a snippet table from given list."
;;   (dolist (snippet snippets-alist)
;;     (puthash (car snippet) (cdr snippet) rattex--snippet-table)))

;; (defvar my/rattex-snippets
;;   (list
;;    '("pd" . (rattex-insert-partial-derivative . rattex-default-math-condition-p))))

;; (defun rattex-default-math-condition-p ()
;;   "Return t if char before the point is alphanumeric"
;;   (let ((inhibit-message t))
;;     (memq (get-char-code-property (char-before) 'general-category)
;; 	  '(Ll Lu Lo Lt Lm Nl Nd No))))

;; (defvar rattex-snippet-max-length 4
;;   "Maximum length of snippet key string.")

;; (defun rattex--snippet-try-key (key length)
;;   "Try to expand snippet from given key."
;;   (let ((action (gethash key rattex--snippet-table nil)))
;;     (if action
;; 	(progn
;; 	  (backward-char length)
;; 	  (if (funcall (cdr action))
;; 	      (forward-char length)
;; 	    (delete-char length)
;; 	    (funcall (car action))
;; 	    nil))
;;       t)))

;; (defun rattex-snippet-try-expand ()
;;   "Try to expand snippet at point"
;;   (interactive)
;;   (let ((move 1)
;; 	(key-end (point)))
;;     (while
;; 	(and
;; 	 (<= move rattex-snippet-max-length)
;; 	 (let* ((key-start (- key-end move))
;; 		(key (buffer-substring-no-properties key-start key-end)))
;; 	   (setq move (1+ move))
;; 	   (if (< key-start 1)
;; 	       nil
;; 	     (rattex--snippet-try-key key (- key-end key-start))))))))

;; (define-minor-mode rattex-mode
;;   "ne pridumal"
;;   :lighter "rttx")

;; (rattex-snippet-toggle-auto)
;; (rattex-snippet-set-table my/rattex-snippets)

;; (defconst rattrap-path (concat user-emacs-directory "pkgs/rattrap/"))
;; (use-package rattrap
;;   :ensure nil
;;   :demand t
;;   :load-path rattrap-path
;;   :config
;;   (defvar my/partiald-rat
;;     (list :action    #'ignore
;; 	  :condition #'rattrap-builtin-cond-alphanum
;; 	  :modifier  ?d
;; 	  :prefixes  '((?s ))))

;;   (defvar my/rattrap-latex-table
;;     (rattrap-make-table
;;      '(("pd" . my/partiald-rat)) )

(define-key emacs-lisp-mode-map (kbd "M-q") #'lisp-fill-paragraph)

(use-package org
  :defer t
  :ensure nil
  :init
  (add-hook 'org-mode-hook #'my/config-org-faces)
  :hook
  (org-mode . display-line-numbers-mode)
  :config

  (setq org-src-window-setup 'current-window)

  ;; Logging
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)
  (setq org-agenda-start-with-log-mode t)

  ;; Agenda
  (setq org-agenda-files
	'("~/org/study/mit-ocw.org"))

  (setq org-agenda-window-setup 'only-window)
  (setq org-agenda-restore-windows-after-quit t)

  (add-hook 'after-save-hook 'my/tangle-config))

;; Me!
(setq user-full-name "Ratibor Siryi"
      user-mail-address "siryi.ra@phystech.edu")

(use-package elfeed
  :defer t
  :config
  (setq elfeed-show-entry-switch 'my/elfeed-show)
  (setq elfeed-feeds
	'(("https://karthinks.com/index.xml" blog emacs)
	  ("https://blogs.ams.org/mathgradblog/feed/" math)
	  ("https://rsshub.app/telegram/channel/miptru" tgch mipt))))

(use-package elpher
  :defer t)

(use-package notmuch
  :defer t
  :init
  (setq mail-user-agent 'notmuch-user-agent))

(setq sendmail-coding-system 'utf-8-unix
      smtpmail-smtp-server "smtp.yandex.ru"
      smtpmail-smtp-service 465
      smtpmail-stream-type 'ssl
      message-send-mail-function #'smtpmail-send-it)

(use-package eww
  :ensure nil
  :config
  (setq browse-url-browser-function #'eww-browse-url))

(use-package rime
  :defer t)

(defun input-method-set-default (im)
  "Set default input method"
  (interactive "sInput method: ")
  (let ((custom-input-method-alist '(("ru" . "russian-computer")
				     ("zh" . "rime"))))
    (setq default-input-method (cdr (assoc im custom-input-method-alist nil)))
    (message (cdr (assoc im custom-input-method-alist nil)))))

(setq default-input-method "russian-computer")

(use-package jinx
  :hook (org-mode text-mode LaTeX-mode)
  :config
  (setq jinx-languages "en_US ru_RU")
  (keymap-local-set "M-$" #'jinx-correct))

(use-package speed-type
  :defer t)

;; (use-package wolfram-mode
;;   :config
;;   (setq wolfram-path "~/.Mathematica/Applications"
;; 	wolfram-program "wolframscript")
;;   (add-to-list 'auto-mode-alist '("\.wls$" . wolfram-mode)))

(use-package xah-wolfram-mode
  :ensure nil
  :demand t
  :load-path "pkgs/xah-wolfram-mode/"
  :config
  (keymap-set xah-wolfram-mode-map "C-c C-c" #'xah-wolfram-run-script))

(use-package crdt)

(use-package electric
  :after prog-mode
  :hook ((prog-mode . electric-pair-mode)))

(use-package prog-mode
  :ensure nil
  :hook
  (prog-mode . display-line-numbers-mode))

(use-package eglot
  :defer t
  :ensure nil
  :hook
  (c++mode . eglot-ensure)
  :config
  (add-to-list 'eglot-server-programs
	       '((c-mode c++-mode)
		 . ("clangd"))))

(use-package magit
  :defer t)

(use-package julia-mode
  :defer t)

(use-package julia-repl
  :defer t)

(use-package yaml-mode
  :defer t)

(use-package nix-mode
  :defer t)

(use-package slime
  :defer t
  :config
  (setq inferior-lisp-program "sbcl"))

(use-package sxhkdrc-mode
  :defer t
  :hook
  (sxhkdrc-mode . display-line-numbers-mode)
  :config
  (add-to-list 'auto-mode-alist `(,(rx "sxhkdrc" string-end) . sxhkdrc-mode)))

(use-package clang-format
  :defer t
  :config
  (declare-function my/clang-format-config nil)
  (defun my/clang-format-config ()
    "My clang-format config. I have an idea to create alternative
package for clang-format, because this one doesn't provide minor
mode and doing strange things with `org-babel-tangle'."
    (keymap-local-set "C-M-\\" #'clang-format-region)
    (keymap-local-set "C-i" #'clang-format)
    (add-hook 'before-save-hook #'clang-format-buffer nil t))

  (add-hook 'c-mode-hook #'my/clang-format-config)
  (add-hook 'c++-mode-hook #'my/clang-format-config))

(use-package gnuplot
  :defer t
  :config
  (autoload 'gnuplot-mode "gnuplot" "Gnuplot major mode" t)
  (autoload 'gnuplot-make-buffer "gnuplot" "open a buffer in gnuplot-mode" t)
  (setq auto-mode-alist (append '(("\\.gp$" . gnuplot-mode)) auto-mode-alist)))

(use-package company
  :hook prog-mode
  :config
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 1))

(use-package rainbow-delimiters
  :defer t
  :after prog-mode
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package markdown-mode)

;; Me
(setq-default fill-column 80)
(setq frame-resize-pixelwise t)
(column-number-mode 1)
(global-hl-line-mode t)
(add-to-list 'auto-mode-alist '("\\.txt$'" . text-mode))
(setenv "GPG_AGENT_INFO" nil)
(add-hook 'after-save-hook
	  'executable-make-buffer-file-executable-if-script-p)

(use-package vertico
  :demand t
  :config
  (vertico-mode))

(use-package marginalia
  :demand t
  :bind
  (:map minibuffer-local-map
	("M-a" . marginalia-cycle))
  :init
  (marginalia-mode))

(use-package orderless
  :demand t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package consult
  :demand t
  :bind
  (("C-c k" . consult-kmacro)
   ("C-c h" . consult-history)
   ("C-c m" . consult-man)
   ("C-c i" . consult-info)
   ("C-x b" . consult-buffer)
   ;; M-g bindings in `goto-map'
   ("M-g e" . consult-compile-error)
   ("M-g m" . consult-mark)
   ;; M-s bindings in `search-map'
   ("M-s G" . consult-grep)
   ("M-s g" . consult-git-grep)
   ("M-s d" . consult-find)
   )
  :init
  (setq xref-show-xrefs-function #'consult-xref))
(message (emacs-init-time))

(use-package exwm
  :init
  (require 'exwm-config))

(defun my/exwm-config ()
  "Jo Gay's exwm config"
  (setq exwm-workspace-number 10)
  (setq exwm-input-global-keys
	`((,(kbd "s-x") . my/exwm-launch-application)
	  (,(kbd "s-w") . exwm-workspace-switch)
	  (,(kbd "s-|") . exwm-reset)

	  (,(kbd "<XF86AudioMute>") . my/exwm-mute-volume)
	  (,(kbd "<XF86AudioRaiseVolume>") . my/exwm-raise-volume)
	  (,(kbd "<XF86AudioLowerVolume>") . my/exwm-lower-volume)))

  (setq exwm-input-simulation-keys
	`((,(kbd "C-b") . [left])
	  (,(kbd "C-f") . [right])
	  (,(kbd "C-p") . [up])
	  (,(kbd "C-n") . [down])
	  (,(kbd "C-a") . [home])
	  (,(kbd "C-e") . [end]))))

(defun my/exwm-mute-volume ()
  "Mute audio"
  (interactive)
  (start-process-shell-command "mute" nil "wpctl set-mute @DEFAULT_AUDIO_SINK@ toggle"))

(defun my/exwm-raise-volume ()
  "Raise volume"
  (interactive)
  (start-process-shell-command "raisev" nil "wpctl set-volume -l 1.5 @DEFAULT_AUDIO_SINK@ 5%+"))

(defun my/exwm-lower-volume ()
  "Lower volume"
  (interactive)
  (start-process-shell-command "lowerv" nil "wpctl set-volume @DEFAULT_AUDIO_SINK@ 5%-"))

(defun my/exwm-launch-application (command)
  (interactive (list (read-shell-command "jo gay: ")))
  (start-process-shell-command command nil command))

(my/exwm-config)
(exwm-enable)

(use-package dired
  :ensure nil
  :config
  (setq dired-create-destination-dirs 'always)
  (setq dired-create-destination-dirs-on-trailing-dirsep t))

;; (use-package minibuffer-line)
;; (use-package mini-modeline
;;   :config

;;   (setq mini-modeline-display-gui-line nil))

(use-package echo-bar)

(use-package eat)
