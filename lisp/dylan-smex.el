;;; dylan-smex.el --- summary -*- lexical-binding: t -*-

;; Author: Dylan Yang
;; Maintainer: Dylan Yang

;;; Commentary:

;;; Code:
(use-package smex
  :commands (smex smex-major-mode-commands)
  :config (smex-initialize))

(use-package ivy
  :diminish ivy-mode
  :demand t
  :config (ivy-mode 1))

(use-package counsel
  :init (counsel-mode 1)
  :diminish ivy-mode counsel-mode
  :bind (("C-S-s" . swiper-all)
         ("C-s" . swiper)
         ("C-c f" . counsel-find-file)
         ("C-c g" . counsel-grep)
         ("C-c j" . counsel-git-grep)
         ("C-x C-r" . counsel-recentf))
  :hook ((after-init . ivy-mode)
         (ivy-mode . counsel-mode))
  :config
  (setq enable-recursive-minibuffers t
        ivy-use-selectable-prompt t
        ivy-height 10
        ivy-use-virtual-buffers t
        ivy-count-format "(%d/%d) "
        ivy-on-del-error-function nil))

(provide 'dylan-smex)

;;; dylan-smex.el ends here
