;; init-dashboard.el --- config dashboard.	-*- lexical-binding: t -*-

;; Copyright (C) 2019-2021 Dylan Yang

;; Author: Dylan Yang <banshiliuli1990@sina.com>
;; URL: https://github.com/zucchiniy/.emacs.d

;;; Commentary:
;;

;;; Code:
(eval-when-compile
  (require 'init-general)
  (require 'init-ui))

(use-package dashboard
  :diminish dashboard-mode
  :general
  (general-define-key
    :states '(normal visual emacs)
    :keymaps 'override
    :prefix "SPC d"
    "h" 'browse-homepage
    "r" 'restore-session
    "s" 'find-custom-file
    "u" 'update-config-and-packages
    "q" 'quit-dashboard)
  :hook (dashboard-mode . (lambda () (setq-local frame-title-format nil)))
  :init
  (dashboard-setup-startup-hook)
  (setq dashboard-banner-logo-title "Dylan's Emacs - Talk is cheap. Show me the code."
        dashboard-startup-banner (expand-file-name
                                  (if (display-graphic-p) "logo.png"
                                    "banner.txt")
                                  user-emacs-directory)
        dashboard-center-content t
        dashboard-vertically-center-content t
        dashboard-show-shortcuts nil
        dashboard-items '((recents   . 10)
                          (bookmarks . 5)
                          (projects  . 5)
                          (agenda    . 5)
                          (registers . 5))
        dashboard-startupify-list '(dashboard-insert-banner
                                    dashboard-insert-newline
                                    dashboard-insert-banner-title
                                    dashboard-insert-newline
                                    dashboard-insert-navigator
                                    dashboard-insert-newline
                                    dashboard-insert-init-info
                                    dashboard-insert-items
                                    dashboard-insert-newline
                                    dashboard-insert-footer)
        dashboard-navigation-cycle t
        dashboard-display-icons-p t
        dashboard-icon-type 'nerd-icons
        dashboard-set-file-icons t
        dashboard-set-heading-icons t
        dashboard-heading-icons '((recents   . "nf-oct-history")
                                  (bookmarks . "nf-oct-bookmark")
                                  (agenda    . "nf-oct-calendar")
                                  (projects  . "nf-oct-briefcase")
                                  (registers . "nf-oct-database"))
        dashboard-item-shortcuts '((recents . "r")
                                   (bookmarks ."m")
                                   (projects . "p")
                                   (agenda . "a")
                                   (registers . "e"))
        dashboard-week-agenda t)
  )

(provide 'init-dashboard)
;;; init-dashboard.el ends here
