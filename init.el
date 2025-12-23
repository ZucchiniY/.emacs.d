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
  (dolist (dir '("core" "modules" "load-lisp"))
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
(require 'core-const)
(require 'core-funcs)

;; Packages
(require 'core-package)

;; Preferences
(require 'core-basis)
(require 'core-hydra)
(require 'core-edit)

(require 'core-keybind)

(require 'core-company)
(require 'core-counsel)
(require 'core-ui)
(require 'core-treemacs)

(require 'core-org)
;; add org-roam config
(require 'core-roam)

;; dashboard
(require 'modules-dashboard)
(require 'modules-magit)
(require 'modules-projectile)

;; 节日提醒
(require 'modules-calendar)

;; treesit
;; (require 'modules-treesit)
;; add novelist tools to Creative novels
(require 'modules-novel)

;; 使用 lspce 作为工具
;; (python rust)
(require 'modules-lsp)

;; development configuration
(require 'modules-rust)

;; add ox-hugo
;; (require 'modules-hugo)

;; json/jsonl mode
;;(require 'modules-json)

;; lua mode
(require 'modules-lua)

(provide 'init)
;;; init.el ends here
