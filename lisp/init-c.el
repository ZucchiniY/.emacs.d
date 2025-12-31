;;; init-c.el --- summary -*- lexical-binding: t -*-

;; Copyright (C) 2019-2026 Dylan Yang

;; Author: Dylan Yang <banshiliuli1990@sina.com>
;; URL: https://github.com/zucchiniy/.emacs.d

;;; Commentary:
;;
;; C/C++ configuration
;;
;;; Code:
(eval-when-compile
  (require 'init-custom))

;; C/C++ Mode
(use-package cc-mode
  :init (setq-default c-basic-offset 4))

(when (dylan-treesit-available-p)
  (use-package c-ts-mode
    :functions dylan-treesit-available-p
    :init
    (setq c-ts-mode-indent-offset 4)

    (when (boundp 'major-mode-remap-alist)
      (add-to-list 'major-mode-remap-alist '(c-mode . c-ts-mode))
      (add-to-list 'major-mode-remap-alist '(c++-mode . c++-ts-mode))
      (add-to-list 'major-mode-remap-alist
                   '(c-or-c++-mode . c-or-c++-ts-mode)))))

(provide 'init-c)
;;; init-c.el ends here
