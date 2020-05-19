(use-package smex
  :commands (smex smex-major-mode-commands)
  :config (smex-initialize))

(use-package ivy
  :diminish ivy-mode
  :demand t
  :config
  (setq ivy-use-virtual-buffers t
        enable-recursive-minibuffers t)
  (ivy-mode 1))

(use-package swiper
  :diminish swiper
  :config (setq search-default-mode nil))

(use-package counsel
  :after (ivy swiper)
  :init (counsel-mode 1)
  :diminish ivy-mode counsel-mode
  :general
  (global-leader
   ":" 'counsel-M-x
   "sS" 'swiper-all
   "ss" 'swiper
   "ff" 'counsel-find-file
   "fr" 'counsel-recentf
   "cr" 'counsel-rg
   "bb" 'counsel-switch-buffer
   "cg" 'counsel-git-grep)
  :hook ((after-init . ivy-mode)
         (ivy-mode . counsel-mode))
  :config
  (setq enable-recursive-minibuffers t
        ivy-use-selectable-prompt t
        ivy-height 10
        ivy-use-virtual-buffers t
        ivy-count-format "(%d/%d) "
        ivy-on-del-error-function nil))

(use-package counsel-org-clock
  :after (counsel org)
  :custom
  (counsel-org-clock-default-action 'counsel-org-clock-clock-dwim-action))

(provide 'core-counsel)
