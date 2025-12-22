;; modules-dashboard.el --- config dashboard.	-*- lexical-binding: t -*-

;; Copyright (C) 2019-2021 Dylan Yang

;; Author: Dylan Yang <banshiliuli1990@sina.com>
;; URL: https://github.com/zucchiniy/.emacs.d

;;; Commentary:
;;

;;; Code:
(require 'core-const)

(use-package dashboard
  :ensure t
  :init
  (setq dashboard-projects-backend 'projectile)
  (setq dashboard-set-heading-icons t
        dashboard-set-file-icons t
        dashboard-center-content t
        dashboard-banner-logo-title "Dylan's Emacs - Talk is cheap. Show me the code."
        ;; dashboard-startup-banner (expand-file-name "logo.png" user-emacs-directory)
        ;; dashboard-startup-banner 'official
        dashboard-items '((recents   . 5)
                          (projects  . 5)
                          (bookmarks . 5)
                          (agenda    . 5)
                          (registers . 5)
                          ))
  (setq dashboard-projects-switch-function 'counsel-projectile-switch-project)

  ;; nerd icons
  (setq dashboard-icon-type 'nerd-icons
        dashboard-display-icons-p t)

  (setq dashboard-week-agenda t
        dashboard-filter-agenda-entry 'dashboard-no-filter-agenda)

  (dashboard-setup-startup-hook)
  )

(provide 'modules-dashboard)
;;; modules-dashboard.el ends here
