;; Me
(setq user-full-name "Ratibor Siryi"
      user-mail-address "siryi.ra@phystech.edu")

(defun my/fontconfig ()
    "font configuration"
  (set-face-attribute 'default nil :font "Noto Sans Mono")
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
(add-hook 'LaTeX-mode-hook
	  '(lambda ()
	     (define-key LaTeX-mode-map (kbd "$") 'self-insert-command)))

(use-package tex
  :ensure auctex)

(add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer)
(add-hook 'LaTeX-mode-hook 'prettify-symbols-mode)
(add-hook 'LaTeX-mode-hook 'display-line-numbers-mode)
(setq auto-mode-alist (cons '("\\.tex$" . latex-mode) auto-mode-alist))

(setq prettify-symbols-unprettify-at-point t)


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

(use-package yaml-mode)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages '(magit auctex use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(put 'upcase-region 'disabled nil)
