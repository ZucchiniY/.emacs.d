;; core-package.el --- Define Package config.	-*- lexical-binding: t -*-

;; Copyright (C) 2019-2021 Dylan Yang

;; Author: Dylan Yang <banshiliuli1990@sina.com>
;; URL: https://github.com/zucchiniy/.emacs.d

;;; Commentary:
;;

;;; Code:
;; Set package archives
(require 'core-variable)

(setq package-archives tuna-elpa)

;; Initialize packages
(unless (bound-and-true-p package--initialized)
  (setq package-enable-at-startup nil)
  (package-initialize))

;; use package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Should set before loading `use-package'

(setq use-package-always-ensure t)
(setq use-package-always-defer t)
(setq use-package-expand-minimally t)
(setq use-package-enable-imenu-support t)

(eval-when-compile
  (require 'use-package))

;; add all-the-icons package
(use-package all-the-icons :defer t)
(use-package diminish :defer t)
(use-package bind-key :defer t)

;; use package-utils to update packages
(use-package package-utils
	     :init
	     (defalias 'upgrade-packages 'package-utils-upgrade-all)
	     (defalias 'upgrade-packages-and-restart 'package-utils-upgrade-all-and-restart))

(declare-function upgrade-packages-and-restart 'init-package)
(defalias 'dylan-update-packages-and-restartup 'upgrade-packages-and-restart)

(provide 'core-package)
;;; core-package.el ends here
