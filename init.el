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
(require 'core-ui)
(require 'core-company)
(require 'core-counsel)
(require 'core-treemacs)

(require 'core-evil)
(require 'core-keybinds)

(require 'core-org)

(require 'modules-magit)

(require 'modules-python)
(require 'modules-web)
(require 'modules-projectile)

(provide 'init)
