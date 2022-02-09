(defun update-load-path (&rest _)
  "Update `load-path'."
  (push (expand-file-name "core" user-emacs-directory) load-path)
  (push (expand-file-name "modules" user-emacs-directory) load-path))
(advice-add #'package-initialize :after #'update-load-path)

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(update-load-path)

(require 'core-package)
(require 'core-variable)
(require 'core-basis)
(require 'core-company)
(require 'core-counsel)
(require 'core-ui)
(require 'core-treemacs)

(require 'core-evil)

(require 'core-org)

(require 'modules-magit)
(require 'modules-projectile)

(require 'modules-python)
(require 'modules-web)
(require 'modules-yaml)

(require 'modules-super-agenda)

;; 新增 plantuml 配置功能
(require 'modules-plantuml)

;; 新增 writeroom-mode
(require 'modules-writer)

;; 重新引入 `lsp-mode`
(require 'modules-lsp)

;; 节日提醒
(require 'modules-calendar)

;; add ox-hugo
(require 'modules-hugo)

;; dashboard
(require 'modules-dashboard)

;; django mode
(require 'modules-django)

;; add ox-hugo
(require 'modules-hugo)

(provide 'init)
;;; init.el ends here
