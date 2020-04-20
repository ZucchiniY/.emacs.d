(use-package rime
  :config
  (setq rime-show-candidate 'posframe)
  :custom
  (rime-librime-root (expand-file-name "librime/dist" user-emacs-directory))
  (rime-emacs-module-header-root (expand-file-name "extends" user-emacs-directory))
  (default-input-method "rime"))

(provide 'modules-rime)
