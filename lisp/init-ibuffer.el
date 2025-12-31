;; init-buffer.el --- Initialize ibuffer configurations.	-*- lexical-binding: t -*-

;; Copyright (C) 2019-2026 Dylan Yang

;; Author: Dylan Yang <banshiliuli1990@sina.com>
;; URL: https://github.com/zucchiniy/.emacs.d

;;; Commentary:
;;
;; IBuffer configurations.
;;

;;; Code:

(eval-when-compile
  (require 'init-custom))

(use-package ibuffer
  :ensure nil
  :bind ("C-x C-b" . ibuffer)
  :init (setq ibuffer-filter-group-name-face '(:inherit (font-lock-string-face bold))))

;; Display icons for buffers
(use-package nerd-icons-ibuffer
  :hook ibuffer-mode
  :init (setq nerd-icons-ibuffer-icon dylan-icon))

;; Group ibuffer's list by project
(use-package ibuffer-project
  :autoload (ibuffer-project-generate-filter-groups ibuffer-do-sort-by-project-file-relative)
  :functions icons-displayable-p
  :hook (ibuffer . (lambda ()
                     "Group ibuffer's list by project."
                     (setq ibuffer-filter-groups (ibuffer-project-generate-filter-groups))
                     (unless (eq ibuffer-sorting-mode 'project-file-relative)
                       (ibuffer-do-sort-by-project-file-relative))))
  :init (setq ibuffer-project-use-cache t)
  :config
  (with-no-warnings
    (defun my-ibuffer-project-group-name (root type)
      "Return group name for project ROOT and TYPE."
      (if (and (stringp type) (> (length type) 0))
          (format "%s %s" type root)
        (format "%s" root)))
    (if (icons-displayable-p)
        (progn
          (advice-add #'ibuffer-project-group-name :override #'my-ibuffer-project-group-name)
          (setq ibuffer-project-root-functions
                `((ibuffer-project-project-root . ,(nerd-icons-octicon "nf-oct-repo" :height 1.2 :face ibuffer-filter-group-name-face))
                  (file-remote-p . ,(nerd-icons-codicon "nf-cod-radio_tower" :height 1.2 :face ibuffer-filter-group-name-face)))))
      (progn
        (advice-remove #'ibuffer-project-group-name #'my-ibuffer-project-group-name)
        (setq ibuffer-project-root-functions
              '((ibuffer-project-project-root . "Project")
                (file-remote-p . "Remote")))))))

(provide 'init-ibuffer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-ibuffer.el ends here
