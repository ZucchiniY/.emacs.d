;; modules-dashboard.el --- config dashboard.	-*- lexical-binding: t -*-

;; Copyright (C) 2019-2021 Dylan Yang

;; Author: Dylan Yang <banshiliuli1990@sina.com>
;; URL: https://github.com/zucchiniy/.emacs.d

;;; Commentary:
;;

;;; Code:
(require 'core-variable)

(use-package dashboard
  :ensure t
  :init
  ;; :config
  (setq dashboard-set-heading-icons t
        dashboard-set-file-icons t
        dashboard-set-init-info t
        dashboard-center-content t
        dashboard-banner-logo-title "Dylan's Emacs - Talk is cheap. Show me the code."
        dashboard-startup-banner (expand-file-name "logo.png" user-emacs-directory)
        dashboard-projects-switch-function 'counsel-projectile-switch-project-by-name
        dashboard-items '((recents . 5)
                          (projects . 5)
                          ;; (agenda . 5)
                          (bookmarks . 5)))

  (dashboard-setup-startup-hook)
  )

(provide 'modules-dashboard)
;;; modules-dashboard.el ends here
