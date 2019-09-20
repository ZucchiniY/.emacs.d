(use-package projectile
  :general
  (global-leader
    "p" 'projectile-command-map)
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

(use-package counsel-projectile
  :after (projectile counsel)
  :general
  (global-leader
    "pp" 'counsel-projectile-switch-project
    "p/" 'counsel-projectile-grep
    "SPC" 'counsel-projectile-find-file))

(provide 'modules-projectile)
