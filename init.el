;;; init.el --- summary -*- lexical-binding: t -*-

;; Author: Dylan Yang
;; Maintainer: Dylan Yang
;; Version: 1.0.0
;; Package-Requires: ()
;; Homepage:
;;; Commentary:

;; 初始化文件

;;; Code:
;; upgrade emacs performance
(setq gc-cons-threshold most-positive-fixnum)

;; Optimize `auto-mode-alist`
(setq auto-mode-case-fold nil)

(unless (or (daemonp) noninteractive init-file-debug)
  ;; Temporarily suppress file-handler processing to speed up startup
  (let ((default-handlers file-name-handler-alist))
    (setq file-name-handler-alist nil)
    ;; Recover handlers after startup
    (add-hook 'emacs-startup-hook
              (lambda ()
                (setq file-name-handler-alist
                      (delete-dups (append file-name-handler-alist default-handlers))))
              101)))

;;
;; Config Load Path
;;

(defun update-load-path (&rest _)
  "Update `load-path'."
  (dolist (dir '("lisp" "load-lisp"))
    (push (expand-file-name dir user-emacs-directory) load-path)))

(defun add-subdirs-to-load-path (&rest _)
  "Recursively add subdirs in `load-lisp' to `load-path'."
  (let ((default-directory (expand-file-name "load-lisp" user-emacs-directory)))
    (normal-top-level-add-subdirs-to-load-path)))

(advice-add #'package-initialize :after #'update-load-path)
(advice-add #'package-initialize :after #'add-subdirs-to-load-path)

(update-load-path)

;; add custom config
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

;; Requisites
(require 'init-const)
(require 'init-funcs)

;; Packages
(require 'init-package)

;; Preferences
(require 'init-base)
(require 'init-general) ;; evil and general

(require 'init-ui)

;; Notes and Diary
(require 'init-org)
(require 'init-roam)  ;; use org-roam to organize notes

;; Version Control
(require 'init-vcs)  ;; magit configuration

;; Dashboard
(require 'init-dashboard)  ;; dashboard configuration

;; Snippets
(require 'init-snippet)  ;; snippet configuration

(provide 'init)
;;; init.el ends here
