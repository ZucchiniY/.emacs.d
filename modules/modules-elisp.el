;; modules-elisp.el --- Define elisp.	-*- lexical-binding: t -*-

;; Copyright (C) 2019-2021 Dylan Yang

;; Author: Dylan Yang <banshiliuli1990@sina.com>
;; URL: https://github.com/zucchiniy/.emacs.d

;;; Commentary:
;;
;; emacs-lisp config.
;;

;;; Code:

(use-package elisp-mode
  :ensure nil
  :bind (:map emacs-lisp-mode-map
              ("C-c C-x" . ielm)
              ("C-c C-c" . eval-defun)
              ("C-c C-b" . eval-buffer)))

(use-package eldoc
  :ensure nil
  :diminish eldoc-mode)

(provide 'modules-elisp)
;;; modules-elisp.el ends here
