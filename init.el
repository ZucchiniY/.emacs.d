;;; init.el --- summary -*- lexical-binding: t -*-

;; Author: Dylan Yang
;; Maintainer: Dylan Yang
;; Version: 1.0.0
;; Package-Requires: ()
;; Homepage:
;; Keywords:

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.


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
  "Recursively add subdirs in 'load-lisp` to `load-path`."
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

;; (require 'init-bookmark)
;; (require 'init-calendar)
(require 'init-dashboard)
;; (require 'init-dired)
;; (require 'init-highlight)
;; (require 'init-ibuffer)
;; (require 'init-kill-ring)
;; (require 'init-workspace)
;; (require 'init-window)
;; (require 'init-treemacs)

;; ;; Notes and Diary
(require 'init-org)
(require 'init-roam)  ;; use org-roam to organize notes
;; (require 'init-novel) ;; use org-novelist to writing novel :BUG 程序会不停的报错 `'Ignoring unknown mode ‘org-novelist-mode’`
(require 'init-markdown) ;; with markdown file

(require 'init-utils)

;; ;; Programming
(require 'init-vcs)
;; (require 'init-check)
(require 'init-lsp) ;; use lspce replace lsp :BUG: 报错栈超用，修改为使用 eglot 进行补全

;; shell and eshell
;; (require 'init-shell)
;; (require 'init-eshell)

;; Development language
;; (require 'init-prog)
;; (require 'init-elisp)
;; (require 'init-c)
(require 'init-rust)
(require 'init-python)
;; (require 'init-web)
;; (require 'init-lua) ;; lua mode

(provide 'init)
;;; init.el ends here
