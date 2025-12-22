;; core-counsel.el --- config counsel and ivy , swiper.	-*- lexical-binding: t -*-

;; Copyright (C) 2019-2021 Dylan Yang

;; Author: Dylan Yang <banshiliuli1990@sina.com>
;; URL: https://github.com/zucchiniy/.emacs.d

;;; Commentary:
;;

;;; Code:
(use-package smex
  :demand t
  :commands (smex smex-major-mode-commands)
  :config (smex-initialize))

(use-package ivy
  :demand t
  :diminish ivy-mode
  :config
  (setf ivy-re-builders-alist '((swiper . ivy--regex-plus)
                                (t . ivy--regex-fuzzy)))
  (setq ivy-use-virtual-buffers t
        enable-recursive-minibuffers t)
  (ivy-mode 1))

(use-package swiper
  :defer 1
  :diminish swiper
  :config (setq search-default-mode nil))

(use-package counsel
  :demand t
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
        ivy-on-del-error-function nil)
  (setq counsel-describe-function-function #'helpful-callable
        counsel-describe-variable-function #'helpful-variable)
       )

(use-package counsel-org-clock
  :after (counsel org)
  :custom
  (counsel-org-clock-default-action 'counsel-org-clock-clock-dwim-action))

(provide 'core-counsel)
;;; core-counsel.el ends here
