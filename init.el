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
(require 'init-custom)

;; Packages
(require 'init-package)

;; Preferences
(require 'init-base)
(require 'init-hydra)
(require 'init-general) ;; evil and general

(require 'init-ui)
(require 'init-edit)
(require 'init-completion)
(require 'init-snippet)

(require 'init-calendar)
(require 'init-dashboard)
;; (require 'init-highlight) ;; FIXED BUG
(require 'init-ibuffer)
(require 'init-kill-ring)
(require 'init-workspace)
;; (require 'init-window)
(require 'init-treemacs)

;; ;; Notes and Diary
(require 'init-org)
(require 'init-roam)  ;; use org-roam to organize notes
(require 'init-novel) ;; use org-novelist to writing novel
(require 'init-markdown) ;; with markdown file

(require 'init-utils)

;; ;; Programming
(require 'init-vcs)
(require 'init-check)
;; (require 'init-lsp) ;; use eglot, elgot-booster and emacs-lisp-booster

;; Development language
;; (require 'init-c)
;; (require 'init-rust)
;; (require 'init-python)
;; (require 'init-web) ;; :FIXED have bug

;; Not Need
;; (require 'init-bookmark)
;; (require 'init-prog)
;; (require 'init-elisp)
;; (require 'init-shell)
;; (require 'init-eshell)
;; (require 'init-dired)

(provide 'init)
;;; init.el ends here
