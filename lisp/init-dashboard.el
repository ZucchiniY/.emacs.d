;; init-dashboard.el --- config dashboard.	-*- lexical-binding: t -*-

;; Copyright (C) 2019-2021 Dylan Yang

;; Author: Dylan Yang <banshiliuli1990@sina.com>
;; URL: https://github.com/zucchiniy/.emacs.d

;;; Commentary:
;;

;;; Code:
(eval-when-compile
  (require 'init-custom)
  (require 'init-general))

(use-package dashboard
    :diminish dashboard-mode
    :general
    (global-leader
      "d d" 'open-dashboard
      "d h" 'browse-homepage
      "d r" 'restore-session
      "d s" 'find-custom-file
      "d u" 'update-config-and-packages)
    (general-define-key
      :states '(normal visual emacs)
      :keymaps 'dashboard-mode-map
      "H" 'browse-homepage
      "R" 'restore-session
      "S" 'find-custom-file
      "U" 'update-config-and-packages
      "q" 'quit-dashboard)
    :hook (dashboard-mode . (lambda () (setq-local frame-title-format nil)))
    :init
    (setq dashboard-banner-logo-title "Dylan's Emacs - Talk is cheap. Show me the code."
          dashboard-startup-banner (or dylan-logo 'official)
          dashboard-page-separator "\n\f\n"
          dashboard-projects-backend 'project-el
          dashboard-path-style 'truncate-middle
          dashboard-path-max-length 60
          dashboard-center-content t
          dashboard-vertically-center-content t
          dastboard-show-shortcuts nil
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
          dashboard-display-icons-p #'icons-displayable-p
          dashboard-set-file-icons dylan-icon
          dashboard-set-heading-icons dylan-icon
          dashboard-heading-icons '((recents   . "nf-oct-history")
                                    (bookmarks . "nf-oct-bookmark")
                                    (agenda    . "nf-oct-calendar")
                                    (projects  . "nf-oct-briefcase")
                                    (registers . "nf-oct-database"))))

(provide 'init-dashboard)
;;; init-dashboard.el ends here
