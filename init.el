;;; init.el --- summary -*- lexical-binding: t -*-

;; Author: Dylan Yang
;; Maintainer: Dylan Yang
;;; Commentary:

;;; Code:
(defun update-load-path (&rest _)
  "Update `load-path'."
  (push (expand-file-name "site-lisp" user-emacs-directory) load-path)
  (push (expand-file-name "lisp" user-emacs-directory) load-path))
(advice-add #'package-initialize :after #'update-load-path)

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(update-load-path)

(require 'dylan-package)
(require 'dylan-basic)

(require 'dylan-org)
(require 'dylan-magit)

(require 'dylan-expansion)
(require 'dylan-company)
(require 'dylan-smex)

(require 'dylan-python)
(require 'dylan-web)
;;(require 'dylan-evil)

(provide 'init)
;;; init.el ends here
