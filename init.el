;;; init.el --- summary -*- lexical-binding: t -*-

;; Author: Dylan Yang
;; Maintainer: Dylan Yang
;; Version: 1.0.0
;; Package-Requires: ()
;; Homepage:
;; Keywords:


;; This file is not part of GNU Emacs

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
(setq gc-cons-threshold (* 100 1024 1024)
      read-process-output-max (* 1024 1024)
      company-idle-delay 0.0
      company-minimum-prefix-length 1
      create-lockfiles nil) 

(defun update-load-path (&rest _)
  "Update `load-path'."
  (push (expand-file-name "core" user-emacs-directory) load-path)
  (push (expand-file-name "modules" user-emacs-directory) load-path))
(advice-add #'package-initialize :after #'update-load-path)

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(update-load-path)

(require 'core-basis)
(require 'core-package)
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

;; lsp python
(require 'modules-lsp-python)

;; 节日提醒
(require 'modules-calendar)

;; add ox-hugo
(require 'modules-hugo)

(provide 'init)
;;; init.el ends here

