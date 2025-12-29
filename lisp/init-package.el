;; init-package.el --- Define Package config.	-*- lexical-binding: t -*-

;; Copyright (C) 2019-2021 Dylan Yang

;; Author: Dylan Yang <banshiliuli1990@sina.com>
;; URL: https://github.com/zucchiniy/.emacs.d

;;; Commentary:
;;

;;; Code:
;; Set package archives and use-package.
(eval-when-compile
  (require 'init-const))

;; (setq package-archives ustc-elpa)
(setq package-archives tuna-elpa)

;; Initialize packages
(unless (bound-and-true-p package--initialized)
  (setq package-enable-at-startup nil)
  (package-initialize))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(setq use-package-always-ensure t
      use-package-always-defer t
      use-package-expand-minimally t
      use-package-enable-imenu-support t)

;; check loaded package and M-x use-package-report
(setq use-package-compute-statistics t)

;; add diminish
(use-package diminish)

;; update GPG keyring for GNU ELPA
(use-package gnu-elpa-keyring-update)

(unless (fboundp 'package-upgrade-all)
  (use-package auto-package-update
    :autoload auto-package-update-now
    :custom
    (auto-package-update-delete-old-versions t)
    (auto-package-update-hide-results t)
    :init (defalias 'package-upgrade-all #'auto-package-update-now)))

(provide 'init-package)
;;; init-package.el ends here
