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


(defun update-load-path (&rest _)
  "Update `load-path'."
  (push (expand-file-name "core" user-emacs-directory) load-path)
  (push (expand-file-name "modules" user-emacs-directory) load-path))
(advice-add #'package-initialize :after #'update-load-path)

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(update-load-path)

(require 'core-package)
(require 'core-variable)
(require 'core-basis)
(require 'core-company)
(require 'core-counsel)
(require 'core-ui)
(require 'core-treemacs)

(require 'core-evil)

(require 'core-org)

(require 'modules-magit)
(require 'modules-projectile)

(require 'modules-python)
(require 'modules-web)
(require 'modules-yaml)

(require 'modules-super-agenda)

;; 新增 plantuml 配置功能
(require 'modules-plantuml)

;; 新增 writeroom-mode
(require 'modules-writer)

;; 重新引入 `lsp-mode`
(require 'modules-lsp)

;; 节日提醒
(require 'modules-calendar)

;; add ox-hugo
(require 'modules-hugo)

;; dashboard
(require 'modules-dashboard)

;; django mode
(require 'modules-django)

;; add ox-hugo
(require 'modules-hugo)

;; add org-roam config
(require 'modules-roam)

(provide 'init)
;;; init.el ends here

