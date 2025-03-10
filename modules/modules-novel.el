;;; modules-novel.el --- summary -*- lexical-binding: t -*-

;; Author: Dylan Yang
;; Maintainer: Dylan Yang
;; Version: 1.0.0
;; Package-Requires: ( )
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

;; 序言、前言 front matter
;; 主故事 main matter
;; 后记 back matter

;;; Code:
(require 'core-org)

(use-package org-novelist
  :load-path "site-lisp/org-novelist"
  :defer 1
  ;; :ensure t
  ;; :after (org org-roam)
  :custom
  (setq org-novelist-language-tag "zh-Hans"
        org-novelist-author "INTJ摸鱼小能手"
        org-novelist-author-email "banshiliuli1990@sina.com"
        org-novelist-automatic-referencing-p nil)
  ;; :diminish org-novelist-mode
  :general
  (general-define-key
   :states '(normal visual)
   :keymaps 'override
   :prefix "SPC l"
   "n" 'org-novelist-new-story ;; 新建小说
   "r" 'org-novelist-rename-story ;; 重命名小说
   "f" 'org-novelist-update-references ;; 更新参考
   "l" 'org-novelist-link-to-story ;; 关联现有小说
   "u" 'org-novelist-unlink-from-story ;; 取消小说关联
   "cc" 'org-novelist-new-character ;; 新建角色
   "cr" 'org-novelist-rename-character ;; 更新角色
   "cd" 'org-novelist-destroy-character ;; 移除角色
   "pc" 'org-novelist-new-place ;; 新增地点
   "pr" 'org-novelist-rename-place ;; 重命名地点
   "pd" 'org-novelist-destroy-place ;; 删除地点
   "oc" 'org-novelist-new-prop ;; 新增物品
   "or" 'org-novelist-rename-prop ;; 重命名物品
   "od" 'org-novelist-destroy-prop ;; 删除物品
   "ac" 'org-novelist-new-chapter ;; 新增章节
   "ar" 'org-novelist-rename-chapter ;; 重命名章节
   "ad" 'org-novelist-destroy-chapter ;; 删除章节
   )
  )
                       

(provide 'modules-novel)
;;; modules-novel.el ends here

