;; init-snippet.el --- Initialize snippet configurations.	-*- lexical-binding: t -*-

;; Author: Dylan Yang
;; URL: https://github.com/zucchiniy/.emacs.d

;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;

;;; Commentary:
;;
;; Snippet configurations.
;;

;;; Code:

;; Yet another snippet extension
(use-package yasnippet
  :diminish yas-minor-mode
  :hook (after-init . yas-global-mode)
  :general
  (general-define-key
   :states 'normal
   :prefix "SPC y" ;; 代码片段相关功能前缀
   :doc "代码片段相关功能"
   "g" 'yas/global-mode        ;; 全局模式
   "m" 'yas/minor-mode         ;; 局部模式
   "e" 'yas-activate-extra-mode ;; 激活额外模式
   "d" 'yas-load-directory     ;; 加载目录
   "f" 'yas-visit-snippet-file ;; 访问片段文件
   "l" 'yas-describe-tables    ;; 描述表格
   "i" 'yas-insert-snippet     ;; 插入片段
   "n" 'yas-new-snippet        ;; 新建片段
   "t" 'yas-tryout-snippet     ;; 尝试片段
   "a" 'yas-reload-all         ;; 重新加载所有
   )
  )

(use-package yasnippet-snippets)

(use-package yasnippet-capf
  :after cape
  :commands yasnippet-capf
  :functions cape-capf-super eglot-completion-at-point my-eglot-capf-with-yasnippet
  :init
  (add-to-list 'completion-at-point-functions #'yasnippet-capf)

  ;; To integrate `yasnippet-capf' with `eglot' completion
  ;; https://github.com/minad/corfu/wiki#making-a-cape-super-capf-for-eglot
  (defun my-eglot-capf-with-yasnippet ()
    (setq-local completion-at-point-functions
                (list
	             (cape-capf-super
		          #'eglot-completion-at-point
		          #'yasnippet-capf))))
  (add-hook 'eglot-managed-mode-hook #'my-eglot-capf-with-yasnippet))

(provide 'init-snippet)
;;; init-snippet.el ends here
