;; core-package.el --- Define Package config.	-*- lexical-binding: t -*-

;; Copyright (C) 2019-2021 Dylan Yang

;; Author: Dylan Yang <banshiliuli1990@sina.com>
;; URL: https://github.com/zucchiniy/.emacs.d

;;; Commentary:
;;

;;; Code:
;; Set package archives and use-package.
(require 'core-basis)

(setq package-archives tuna-elpa)

;; Initialize packages
(unless (bound-and-true-p package--initialized)
  (setq package-enable-at-startup nil)
  (package-initialize))

;; use package
(eval-when-compile
  (add-to-list 'load-path "site-lisp/use-package")
  (require 'use-package))

;; abbrev-mode abbreviation file-name
(use-package abbrev
  :diminish abbrev-mode
  :config
  (if (file-exists-p abbrev-file-name)
      (quietly-read-abbrev-file)))
                     
;; add all-the-icons package
(use-package all-the-icons
  :load-path "site-lisp/all-the-icons"
  :defer t)

(use-package diminish :defer t)
(use-package bind-key :defer t)

;; use package-utils to update packages
(use-package package-utils
	     :init
	     (defalias 'upgrade-packages 'package-utils-upgrade-all)
	     (defalias 'upgrade-packages-and-restart 'package-utils-upgrade-all-and-restart))

(use-package no-littering
  :init
  (setq no-littering-etc-directory (expand-file-name "config/" user-emacs-directory)
	    no-littering-var-directory (expand-file-name "data/" user-emacs-directory)))

(use-package which-key
  :diminish which-key-mode
  :hook (after-init . which-key-mode))

(use-package htmlize)

(use-package yasnippet
  :diminish yas-minor-mode
  :hook (after-init . yas-global-mode)
  :config (use-package yasnippet-snippets))

(use-package expand-region
  :bind ("C-=" . er/expand-region))

(use-package smart-region
  :hook (after-init . smart-region-on))

(declare-function upgrade-packages-and-restart 'init-package)
(defalias 'dylan-update-packages-and-restartup 'upgrade-packages-and-restart)

(use-package recentf
  :defer t
  :config
  (recentf-mode t))

(provide 'core-package)
;;; core-package.el ends here
