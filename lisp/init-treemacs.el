;; init-treemacs.el --- use treemace.	-*- lexical-binding: t -*-

;; Copyright (C) 2019-2026 Dylan Yang

;; Author: Dylan Yang <banshiliuli1990@sina.com>
;; URL: https://github.com/zucchiniy/.emacs.d

;;; Commentary:
;;
;; Treemace configuration
;; A tree layout file explorer.

;;; Code:
(eval-when-compile
  (require 'init-custom))

(use-package treemacs
  :commands (treemacs-follow-mode
             treemacs-filewatch-mode
             treemacs-git-mode)
  :custom-face
  (cfrs-border-color ((t (:inherit posframe-border))))
  :defer 2
  :config
  (setq treemacs-collapse-dirs           (if treemacs-python-executable 3 0)
        treemacs-missing-project-action  'remove
        treemacs-user-mode-line-format   'none
        treemacs-sorting                 'alphabetic-asc
        treemacs-follow-after-init       t
        treemacs-width                   30
        treemacs-no-png-images           (not dylan-icon))

  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t)
  (pcase (cons (not (null (executable-find "git")))
               (not (null (executable-find "python3"))))
    (`(t . t)
     (treemacs-git-mode 'deferred))
    (`(t . _)
     (treemacs-git-mode 'simple)))

  (use-package treemacs-nerd-icons
    :demand t
    :functions treemacs-load-theme
    :config (treemacs-load-theme "nerd-icons"))

  (use-package treemacs-magit
    :hook ((magit-post-commit
            git-commit-post-finish
            magit-post-stage
            magit-post-unstage)
           . treemacs-magit--schedule-update))

  (use-package treemacs-tab-bar
    :demand t
    :functions treemacs-set-scope-type
    :config (treemacs-set-scope-type 'Tabs))
  :general
  (general-define-key
   :states 'normal
   :keymaps 'override
   :prefix "SPC t"
   "t" 'treemacs
   "b" 'treemacs-bookmark
   "s" 'treemacs-select-window
   "a" 'treemacs-delete-other-windows
   "f" 'treemacs-find-file
   "o" 'treemacs-find-tag
   ))

(provide 'init-treemacs)
;;; init-treemacs.el ends here
