;; core-evil.el --- config evil and evil keybind.	-*- lexical-binding: t -*-

;; Copyright (C) 2019-2021 Dylan Yang

;; Author: Dylan Yang <banshiliuli1990@sina.com>
;; URL: https://github.com/zucchiniy/.emacs.d

;;; Commentary:
;;

;;; Code:
(use-package evil-collection
  :after evil
  :config
  (evil-collection-init)
  :custom (evil-collection-setup-minibuffer t))

(provide 'core-keybinds)
