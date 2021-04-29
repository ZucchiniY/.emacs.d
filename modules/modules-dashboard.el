;; modules-dashboard.el --- config dashboard.	-*- lexical-binding: t -*-

;; Copyright (C) 2019-2021 Dylan Yang

;; Author: Dylan Yang <banshiliuli1990@sina.com>
;; URL: https://github.com/zucchiniy/.emacs.d

;;; Commentary:
;;

;;; Code:
(require 'core-variable)

(use-package dashboard
  :defer t
  :init
  (dashboard-setup-startup-hook)
  :config
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-center-content t)
  (setq dashboard-banner-logo-title "Welcome to Dylan's Emacs"
        dashboard-startup-banner (expand-file-name "logo.png" user-emacs-directory)
        dashboard-items '((recents . 5)
                          (projects . 5)
                          (agenda . 5)
                          (bookmarks . 5))))

(provide 'modules-dashboard)
;;; modules-dashboard.el ends here
