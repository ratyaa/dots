;;; -*- lexical-binding: t -*-
(setq gc-cons-threshold most-positive-fixnum)
(setq load-prefer-newer noninteractive)

(defvar current-init-timestamp before-init-time)

(push '(tool-bar-lines . 0) default-frame-alist)
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars . nil) default-frame-alist)
(push '(left-fringe . 12) default-frame-alist)
(push '(right-fringe . 0) default-frame-alist)

(setq inhibit-startup-screen t)
(setq initial-buffer-choice nil)
(setq initial-major-mode 'fundamental-mode)

(use-package use-package)
(setq use-package-always-ensure t)
(setq use-package-always-defer t)

(setq user-emacs-directory (file-name-directory (file-truename load-file-name)))
(setq custom-file (concat user-emacs-directory "custom.el"))

(defun my/reset-after-init ()
  "Reset parameters used in early-init for faster startup to
something reasonable."
  (setq gc-cons-threshold (* 16 1024 1024)))

(add-hook 'after-init-hook #'my/reset-after-init)
