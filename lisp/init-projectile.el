;;; init-projectile.el --- summary -*- lexical-binding: t -*-

;; Author: Dylan Yang
;; Maintainer: Dylan Yang
;; Version: 1.0.0

;;; Commentary:

;;; Code:
(use-package projectile
  :general
  (global-leader
    "p" 'projectile-command-map)
  :hook (after-init . projectile-mode)
  :init
  (setq projectile-mode-line-prefix ""
        projectile-sort-order 'recentf
        projectile-enable-caching t
        projectile-use-git-grep t)
  :config
  (projectile-mode 1)
  (projectile-update-mode-line)
  )

(use-package counsel-projectile
  :after (projectile counsel)
  :general
  (global-leader
    "pp" 'counsel-projectile-switch-project
    "p/" 'counsel-projectile-grep
    "SPC" 'counsel-projectile-find-file))

(provide 'init-projectile)

;;; init-projectile.el ends here
