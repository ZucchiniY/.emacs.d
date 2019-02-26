;;; dylan-projectile.el --- summary -*- lexical-binding: t -*-

;; Author: Dylan Yang
;; Maintainer: Dylan Yang
;;; Commentary:

;;; Code:

(use-package projectile
  :bind (:map projectile-mode-map
              ("C-c p" . projectile-command-map))
  :hook (after-init . projectile-mode)
  :init
  (setq projectile-mode-line-prefix ""
        projectile-sort-order 'recentf
        projectile-use-git-grep t)
  :config
  (projectile-update-mode-line)
  (let ((command
         (let ((rg-cmd ""))
           (dolist (dir projectile-globally-ignored-directories)
             (setq rg-cmd (format "%s --glob '!%s'" rg-cmd dir)))
           (concat "rg -0 --files --color=never --hidden" rg-cmd))))
    (setq projectile-generic-command command))
  (when sys/winntp
    (setq projectile-indexing-method 'alien
          projectile-enable-caching nil)
    (setq projectile-git-submodule-command nil)))

(provide 'dylan-projectile)

;;; dylan-projectile.el ends here
