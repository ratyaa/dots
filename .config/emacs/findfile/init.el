(use-package emacs
  :ensure nil
  :config
  (load (concat user-emacs-directory "personal-lib")))

(defvar my/xdg-open-extensions
  (list "pdf")
  "List of filename extensions which need to be opened via xdg-open.")

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
  :config
  (setq completion-styles '(orderless basic))
  (setq completion-category-overrides '((file (styles basic partial-completion)))))
