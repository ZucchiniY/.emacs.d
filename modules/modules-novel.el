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
(require 'core-roam)

(use-package org-novelist
  :load-path "site-lisp/org-novelist"
  :bind ("<f5>" . hydra-org-novelist/body)
  :defer 1
  :init
  (setq org-novelist-language-tag "zh-Hans"
        org-novelist-author "INTJ摸鱼小能手"
        org-novelist-author-email "banshiliuli1990@sina.com"
        org-novelist-automatic-referencing-p nil)
  :hydra (hydra-org-novelist (:color blue :hint nil)
                             "
^故事^        ^角色^        ^地点^        ^物品^        ^章节^
---------------------------------------------------------------------
_n_: 新增故事 _c_: 新增角色 _p_: 新增地点 _s_: 新增物品 _a_: 新增章节
_r_: 重命名   _d_: 重命名   _q_: 重命名   _t_: 重命名   _b_: 重命名
_l_: 关联小说 _e_: 移除角色 _v_: 移除地点 _w_: 移除物品 _d_: 移除章节
_u_: 取消关联
_f_: 更新参考
"
                             ("n" org-novelist-new-story) ;; 新建小说
                             ("r" org-novelist-rename-story) ;; 重命名小说
                             ("f" org-novelist-update-references) ;; 更新参考
                             ("l" org-novelist-link-to-story) ;; 关联现有小说
                             ("u" org-novelist-unlink-from-story) ;; 取消小说关联
                             ("c" org-novelist-new-character) ;; 新建角色
                             ("d" org-novelist-rename-character) ;; 更新角色
                             ("e" org-novelist-destroy-character) ;; 移除角色
                             ("p" org-novelist-new-place) ;; 新增地点
                             ("q" org-novelist-rename-place) ;; 重命名地点
                             ("v" org-novelist-destroy-place) ;; 删除地点
                             ("s" org-novelist-new-prop) ;; 新增物品
                             ("t" org-novelist-rename-prop) ;; 重命名物品
                             ("w" org-novelist-destroy-prop) ;; 删除物品
                             ("a" org-novelist-new-chapter) ;; 新增章节
                             ("b" org-novelist-rename-chapter) ;; 重命名章节
                             ("d" org-novelist-destroy-chapter) ;; 删除章节
                             )
  )

(provide 'modules-novel)
;;; modules-novel.el ends here

