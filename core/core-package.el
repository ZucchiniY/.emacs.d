;; core-package.el --- Define Package config.	-*- lexical-binding: t -*-

;; Copyright (C) 2019-2021 Dylan Yang

;; Author: Dylan Yang <banshiliuli1990@sina.com>
;; URL: https://github.com/zucchiniy/.emacs.d

;;; Commentary:
;;

;;; Code:
;; Set package archives and use-package.
(require 'core-basis)

(setq package-archives ustc-elpa)

;; Initialize packages
(unless (bound-and-true-p package--initialized)
  (setq package-enable-at-startup nil)
  (package-initialize))

;; use package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(setq use-package-always-ensure t)
(setq use-package-always-defer t)
(setq use-package-expand-minimally t)
(setq use-package-enable-imenu-support t)

(use-package diminish :defer t)
(use-package bind-key :defer t)

;; abbrev-mode abbreviation file-name
(use-package abbrev
  :ensure nil
  :diminish abbrev-mode
  :config
  (if (file-exists-p abbrev-file-name)
      (quietly-read-abbrev-file)))

;; add all-the-icons package
(use-package all-the-icons
  :if (display-graphic-p)
  :load-path "site-lisp/all-the-icons")

;; add nerd icons
(use-package nerd-icons)

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
  :hook (after-init . which-key-mode)
  :config (which-key-mode))

(use-package htmlize :defer t)

(use-package yasnippet
  :diminish yas-minor-mode
  :hook (after-init . yas-global-mode)
  :config
  (use-package yasnippet-snippets))

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
