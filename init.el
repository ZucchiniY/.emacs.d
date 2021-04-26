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
;; (require 'core-keybinds)

(require 'core-org)

(require 'modules-magit)
(require 'modules-projectile)

;; (require 'modules-python)
(require 'modules-web)
(require 'modules-yaml)

(require 'modules-super-agenda)

;; (require 'modules-rime)

;; 新增 plantuml 配置功能
(require 'modules-plantuml)

;; 新增 writeroom-mode
(require 'modules-writer)

;; 语言上要增加 Python，Vue 的支持，所以重新引入 `lsp-mode` 进行使用。
(require 'modules-lsp)

;;
(require 'modules-python)
;; 新增支持查看股票
;; (require 'modules-stock)
;; 节日提醒
(require 'modules-calendar)

(provide 'init)
