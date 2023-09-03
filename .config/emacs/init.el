;; Me
(setq user-full-name "Ratibor Siryi"
      user-mail-address "siryi.ra@phystech.edu")

(setq-default fill-column 80)

(set-frame-parameter nil 'alpha-background 80)
(add-to-list 'default-frame-alist '(alpha-background . 80))

(setq frame-resize-pixelwise t)
(fringe-mode '(12 . 0))

(defvar hyperified-chars ""
  "List of keys for which combination of `C-M-S-s-' applies as hyper modifier `H-'.")

(dolist (key (string-to-list hyperified-chars))
  (define-key key-translation-map
    (kbd (concat "C-M-S-s-" (string key)))
    (kbd (concat "H-" (string key)))))

(defun my/fontconfig ()
    "font configuration"
  (set-face-attribute 'default nil :font (concat "JetBrainsMono Nerd Font Mono-" (getenv "EMACS_FONT_SIZE")))
  (set-face-attribute 'fixed-pitch nil :font (concat "JetBrainsMono Nerd Font Mono-" (getenv "EMACS_FONT_SIZE")))
  (set-face-attribute 'variable-pitch nil :font "EB Garamond" :height 140)
  (set-fontset-font t 'han "Noto Sans CJK SC")
  (set-fontset-font t 'han "Noto Sans CJK TC" nil 'append))

(defun my/ui-init ()
  "ui configuration placed here"
  (menu-bar-mode -1)
  (toggle-scroll-bar -1)
  (tool-bar-mode -1)
  (load-theme 'modus-vivendi t)
  (my/fontconfig))

(let ((hook (if (daemonp)
		'server-after-make-frame-hook
	      'after-init-hook)))
  (add-hook hook #'my/ui-init))
  
;; --- From doom emacs' source code:
;; These two functions don't exist in terminal Emacs, but some Emacs packages
;; (internal and external) use it anyway, leading to void-function errors. I
;; define a no-op substitute to suppress them.
(unless (fboundp 'define-fringe-bitmap)
  (fset 'define-fringe-bitmap #'ignore))
(unless (fboundp 'set-fontset-font)
  (fset 'set-fontset-font #'ignore))

;; Disable customize interface
(dolist (customize-functions
	 '(customize-option
	   customize-browse
	   customize-group
	   customize-face
	   customize-rogue
	   customize-saved
	   customize-apropos
	   customize-changed
	   customize-unsaved
	   customize-variable
	   customize-set-value
	   customize-customized
	   customize-set-variable
	   customize-apropos-faces
	   customize-save-variable
	   customize-apropos-groups
	   customize-apropos-options
	   customize-changed-options
	   customize-save-customized
	   customize-themes))
  (put customize-functions 'disabled
       "Disable `customize' functionality."))

;; Package management
(require 'package)
(package-initialize)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/") t)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)

(put 'scroll-left 'disabled nil)

(use-package electric
  :hook ((prog-mode . electric-pair-mode)))

(use-package tex
  :ensure auctex)

(add-to-list 'TeX-view-program-list
	     '("my-pdf-viewer"
	       ("zathura-float %o"
		(mode-io-correlate
		 " --synctex-forward %n:0:\"%(project-dir)tex/main.tex\" -x \"emacsclient +%{line} %{input}\""))
	       "zathura-float"))
	     
(add-to-list 'TeX-view-program-selection '(output-pdf "my-pdf-viewer"))

(eval-after-load "tex"
  '(progn
     (make-local-variable (defvar project-dir nil))
     (add-to-list 'TeX-expand-list
		  '("%(project-dir)" (lambda nil project-dir)))
     (add-to-list 'TeX-command-list
		  '("make"
		    "make -C %(project-dir)"
		    TeX-run-shell nil
		    (latex-mode doctex-mode)
		    :help "Build project via `make'"))))
;;      (add-to-list 'TeX-command-list
;; 		  '("latex-build-project"
;; 		    "%`%l%(mode)%' %(output-dir) %(project-dir)tex/main.tex"
;; 		    TeX-run-TeX nil
;; 		    (latex-mode doctex-mode)
;; 		    :help "Build LaTeX project"))))

(defun my/LaTeX-setup ()
  "My configuration to LaTeX-mode"
  (setq project-dir (file-truename (locate-dominating-file (buffer-file-name) "Makefile")))
  (setq TeX-command-extra-options "--synctex=1")
  
  ;; Modes
  (TeX-fold-mode t)
  (TeX-source-correlate-mode t)
  (display-line-numbers-mode t)
  (delete '("--" . 8211) tex--prettify-symbols-alist)
  (prettify-symbols-mode t)
  (flymake-mode t)
 '(TeX-fold-buffer t)
  (if project-dir
      (progn
	(setq TeX-output-dir (concat project-dir "build"))
	(setq TeX-master (concat project-dir "tex/main.tex"))
	;; (setq TeX-command-default "latex-build-project")
	)))

(add-hook 'LaTeX-mode-hook 'my/LaTeX-setup)
(add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer)

(setq TeX-electric-math '("\\(" . "\\)"))
(setq TeX-source-correlate-start-server t)
(setq auto-mode-alist (cons '("\\.tex$" . latex-mode) auto-mode-alist))
(setq prettify-symbols-unprettify-at-point t)
(setq TeX-parse-self t)
(setq japanese-TeX-error-messages nil)

(use-package yasnippet
  :config
  (setq yas-snippet-dirs '("~/.snippets"))
  (setq yas-triggers-in-field t)
  (yas-global-mode 1))

(use-package gnuplot)
(autoload 'gnuplot-mode "gnuplot" "Gnuplot major mode" t)
(autoload 'gnuplot-make-buffer "gnuplot" "open a buffer in gnuplot-mode" t)
(setq auto-mode-alist (append '(("\\.gp$" . gnuplot-mode)) auto-mode-alist))

(use-package rime)

(defun input-method-set-default (im)
  "Set default input method"
  (interactive "sInput method: ")
  (let ((custom-input-method-alist '(("ru" . "russian-computer")
				     ("zh" . "rime"))))
    (setq default-input-method (cdr (assoc im custom-input-method-alist nil)))
    (message (cdr (assoc im custom-input-method-alist nil)))))


(setq default-input-method "russian-computer")

(use-package clang-format)

(defun clang-format-config ()
  "clang-format configuration"
  (local-set-key (kbd "C-M-\\") 'clang-format-region)
  (local-set-key (kbd "C-i") 'clang-format)
  (add-hook 'before-save-hook #'clang-format-buffer nil t))

(add-hook 'c++-mode-hook #'clang-format-config)
(add-hook 'c-mode-hook #'clang-format-config)

(column-number-mode 1)

(use-package magit)

(use-package erc
  :commands erc
  :config
  (setq
   erc-server "irc.libera.chat"
   erc-nick "ratyaa"
   erc-user-full-name "Ratibor Siryi"
   erc-autojoin-channels-alist '(("irc.libera.chat" "#emacs" "#math" "#physics" "#latex"))
   erc-fill-column 120
   erc-auto-query 'bury
   erc-join-buffer 'bury
   erc-interpret-mirc-color t
   erc-rename-buffers t
   erc-lurker-hide-list '("JOIN" "PART" "QUIT")
   erc-track-exclude-types '("JOIN" "NICK" "QUIT" "MODE")
   erc-track-enable-keybindings
   erc-fill-function 'erc-fill-static
   erc-fill-static-center 20))

(use-package julia-mode)
(use-package julia-repl)

(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(global-hl-line-mode t)

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "H-p") '(message "pivo"))

(use-package eglot)

(add-to-list 'eglot-server-programs
	     '((c-mode c++-mode)
		      . ("clangd")))

(add-hook 'c++mode-hook #'eglot-ensure)

(use-package company)
(add-hook 'prog-mode-hook 'company-mode)
(setq company-idle-delay 0)
(setq company-minimum-prefix-length 1)


(use-package languagetool
  :ensure t
  :commands (languagetool-check
             languagetool-clear-suggestions
             languagetool-correct-at-point
             languagetool-correct-buffer
             languagetool-set-language
             languagetool-server-mode
             languagetool-server-start
             languagetool-server-stop))

(setq languagetool-java-arguments
      '("-Dfile.encoding=UTF-8"
        "-cp" "/usr/share/languagetool:/usr/share/java/languagetool/*")
      languagetool-console-command "org.languagetool.commandline.Main"
      languagetool-server-command "org.languagetool.server.HTTPServer")

(use-package jinx
  :bind (("M-$" . jinx-correct)
         ("C-M-$" . jinx-languages)))

(add-to-list 'auto-mode-alist
	     '("\\.txt\\'" . (lambda ()
			       (jinx-mode)
			       (message "OK"))))

;; Org -----------------------------------------------------------------------------
(setq org-agenda-files '("~/notes"))

(global-set-key (kbd "C-c l") #'org-store-link)
(global-set-key (kbd "C-c a") #'org-agenda)
(global-set-key (kbd "C-c c") #'org-capture)

(setq org-log-done t)
(setq org-goto-auto-isearch nil)
(setq org-format-latex-options
      '(:foreground default
		    :background default
		    :scale 2.0
		    :html-foreground "Black"
		    :html-background "Transparent"
		    :html-scale 1.0
		    :matchers
		    ("begin" "$1" "$" "$$" "\\(" "\\[" "ee")))

(use-package olivetti)
(add-hook 'org-mode-hook
	  (lambda ()
	    (olivetti-mode)
	    (define-key olivetti-mode-map (kbd "C-c |") nil)))

(setq org-latex-create-formula-image-program 'imagemagick)

(setq org-format-latex-header "
\\documentclass[leqno]{article}
\\usepackage[usenames]{color}
[PACKAGES]
[DEFAULT-PACKAGES]
\\pagestyle{empty}             % do not remove
% The settings below are copied from fullpage.sty
\\setlength{\\textwidth}{\\paperwidth}
\\addtolength{\\textwidth}{-8.2cm}
\\setlength{\\oddsidemargin}{1.5cm}
\\addtolength{\\oddsidemargin}{-2.54cm}
\\setlength{\\evensidemargin}{\\oddsidemargin}
\\setlength{\\textheight}{\\paperheight}
\\addtolength{\\textheight}{-\\headheight}
\\addtolength{\\textheight}{-\\headsep}
\\addtolength{\\textheight}{-\\footskip}
\\addtolength{\\textheight}{-3cm}
\\setlength{\\topmargin}{1.5cm}
\\addtolength{\\topmargin}{-2.54cm}

\\usepackage[intlimits]{amsmath}
\\usepackage{amssymb}
\\usepackage{mathtools}
\\usepackage{mathtext}
\\usepackage{xfrac}
\\usepackage{lipsum}
\\usepackage[labelfont=bf,font=footnotesize,justification=centering]{caption}
\\usepackage{enumitem}
\\DeclareMathOperator{\\sinc}{sinc}
\\DeclarePairedDelimiter\\ceil{\\lceil}{\\rceil}
\\DeclarePairedDelimiter\\floor{\\lfloor}{\\rfloor}
\\newcommand{\\Rnum}[1]{\\uppercase\\expandafter{\\romannumeral #1\\relax}}
\\newcommand{\\cg}{\\textsl{g}} % removed \\textnormal before \\textsl
\\newcommand{\\df}{\\mathrm{d}}")

;; (let* ((variable-tuple '(:font "EB Garamond"))
;;        (base-font-color (face-foreground 'default nil 'default))
;;        (headline `(:inherit default :weight bold :foreground ,base-font-color)))
;;   (custom-theme-set-faces
;;    'user
;;    `(org-level-8 ((t (,@headline ,@variable-tuple))))
;;    `(org-level-7 ((t (,@headline ,@variable-tuple))))
;;    `(org-level-6 ((t (,@headline ,@variable-tuple))))
;;    `(org-level-5 ((t (,@headline ,@variable-tuple))))
;;    `(org-level-4 ((t (,@headline ,@variable-tuple))))
;;    `(org-level-3 ((t (,@headline ,@variable-tuple :height 1.25))))
;;    `(org-level-2 ((t (,@headline ,@variable-tuple :height 1.5))))
;;    `(org-level-1 ((t (,@headline ,@variable-tuple :height 1.75))))
;;    `(org-document-title ((t (,@headline ,@variable-tuple :height 2.0 :underline nil))))
;;    '(org-block ((t (:inherit fixed-pitch))))
;;    '(org-code ((t (:inherit (shadow fixed-pitch)))))))
;; (add-hook 'org-mode-hook 'variable-pitch-mode)

;; (defun my/get-latex-element-at-point ()
;;   "Get or construct new org-element with type `latex-environment' at point."
;;   (let ((element (org-element-at-point)))
;;     (unless (and (eq (org-element-type element) 'latex-environment)
;; 		 (org-src--on-datum-p element))
;;       (let ((position (+ 0 (point))))
;; 	(if (> (point) (buffer-size))
;; 	    (newline))
;; 	(org-element-set-element
;; 	 element
;; 	 (list 'latex-environment
;; 	        (list :begin position
;; 		      :end position
;; 		      :value ""
;; 		      :post-blank (org-element-property :post-blank element)
;; 		      :post-affiliated position
;; 		      :parent nil)))))
;;     (element)))

;; (defun my/org-insert-or-edit-latex ()
;;   "Edit or insert latex environment at point."
;;   (let ((element (my/get-latex-element-at-point)))
;;     (org-src--edit-element
;;      element
;;      (org-src--construct-edit-buffer-name (buffer-name) "LaTeX environment")
;;      (org-src-get-lang-mode "latex")
;;      t)
;;     (let ((new-element (my/get-latex-element-at-point)))
;;       (not (= (org-element-property :value element)
;; 	      (org-element-property :value new-element))))))

;; (add-hook 'org-mode-hook
;; 	  (lambda ()
;; 	    (local-set-key (kbd "C-c e") 'my/org-insert-or-edit-latex)))
;; Org -----------------------------------------------------------------------------

(use-package yaml-mode)

(setenv "GPG_AGENT_INFO" nil)

;; (setq user-full-name "Ratibor Siryi"
;;       user-mail-address "siryi.ra@phystech.edu"
;;       send-mail-function 'smtpmail-send-it
;;       smtpmail-smtp-server "smtp.yandex.ru"
;;       smtpmail-stream-type 'starttls
;;       smtpmail-smtp-service 587
;;       gnus-select-method
;;       '(nnimap "yandex"
;; 	       (nnimap-address "imap.yandex.ru")
;; 	       (nnimap-server-port 993)
;; 	       (nnimap-stream ssl)))

(use-package notmuch)

(use-package speed-type)

(use-package sxhkdrc-mode)
(add-hook 'sxhkdrc-mode-hook 'display-line-numbers-mode)
(add-to-list 'auto-mode-alist `(,(rx "sxhkdrc" string-end) . sxhkdrc-mode))

(use-package slime)
(setq inferior-lisp-program "sbcl")

(use-package nix-mode)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(rust-mode gnu-elpa-keyring-update slime magit auctex use-package))
 '(send-mail-function 'sendmail-send-it))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(put 'upcase-region 'disabled nil)
(put 'LaTeX-narrow-to-environment 'disabled nil)
(put 'TeX-narrow-to-group 'disabled nil)
