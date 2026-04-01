;;; init-roam.el --- summary -*- lexical-binding: t -*-

;; Author: Dylan Yang
;; Maintainer: Dylan Yang
;; Version: 1.1.0
;; Package-Requires: (org-mode)
;; Homepage: https://github.com/zucchiniy
;; Keywords: org mode, roam, notes

;;; Commentary:

;; org-roam 对标 roam Research 工具，提供了友好的交互功能，用来构建个人知识网络可视化界面。
;;
;; 利用 orgmode 非常强大的链接功能，配置 org-roam 构建笔记模板。

;; recyle: 将 org-id 相关配置迁移到 init-org.el 配置下

;;; Code:
(require 'init-org)
(require 'org-id)

(use-package emacsql
  :defer 1
  :ensure t)

(use-package org-roam
  :load-path "load-lisp/org-roam"
  :after (org emacsql)
  :defer 2
  :diminish org-roam-mode
  :general
  (general-define-key
   :states '(normal visual emacs)
   :keymaps 'override
   :prefix "SPC n"
   "a" 'org-roam-alias-add
   "c" 'org-roam-capture
   "f" 'org-roam-node-find
   "g" 'org-id-get-create
   "i" 'org-roam-node-insert
   "l" 'org-roam-buffer-toggle
   "R" 'org-roam-ref-add
   "r" 'org-roam-refilg
   "s" 'org-roam-db-sync
   "t" 'org-roam-tag-add
   "U" 'org-id-update-id-locations
   )
  :custom
  ;; 解决 org-roam-ui 仅显示一个 Tag 问题
  (org-roam-database-connector 'sqlite-builtin)
  :config
  ;; (setq org-roam-database-connector 'sqlite-builtin)
  (setq org-roam-directory (expand-file-name (concat org-directory "/roam"))
        org-roam-db-gc-threshold most-positive-fixnum
        org-roam-completion-everywhere t
        org-roam-dailies-directory "projects/"
        org-roam-dailies-capture-templates
        '(("d" "daily" entry
           "* TodoList\n* Review\n%?"
           :target (file+head "%<%Y-%m-%d>.org"
                              "#+title: %<%Y-%m-%d>\n")
           :jump-to-captured t
           :unnarrowed t)
          ("w" "weekly" entry
           "* Weekly Review\n%?"
           :target (file+head "%<%Y-W%W>.org"
                              "#+title: %<%Y-W%W>\n")
           :jump-to-captured t
           :unnarrowed t)
          ("m" "monthly" entry
           "* Monthly\n%?"
           :target (file+head "%<%Y-%m>.org"
                              "#+title: %<%Y-%m>\n")
           :jump-to-captured t
           :unnarrowed t)
          )
        org-roam-capture-templates
        '(("p" "项目")
          ("po" "OKR" checkitem
           "- [ ] %?"
           :target (file+head "projects/%<%Y>OKR.org"
                              "#+title: %<%Y>OKR")
           :empty-lines 1
           :jump-to-captured t
           :unnarrowed t
           )
          ("pi" "收集箱" plain
           "* [#b] %{title} &?"
           :target (file+head "projects/%<%Y>Inboxs.org"
                              "#+title: %<%Y>收集箱")
           :empty-lines 1
           :jump-to-captured t
           :unnarrowed t
           )
          ("a" "领域" plain
           "#+filetags:\n%?"
           :target (file+head "areas/%<%Y%m%d%H%M%S>-${slug}.org"
                              "#+title: ${title}")
           :empty-lines 1
           :jump-to-captured t
           :unnarrowed t
           )
          ("r" "资源" plain
           "#+filetags:\n%?"
           :target (file+head "resources/%<%Y%m%d%H%M%S>-${slug}.org"
                              "#+title: ${title}")
           :empty-lines 1
           :jump-to-captured t
           :unnarrowed t
           )
          )
        )
  ;; 在 org-roam-node-find 时展示的方案
  (setq org-roam-node-display-template
        (concat "${title:*} "
                (propertize "${tags:50}" 'face 'org-tag)))
  (org-roam-db-autosync-mode)
  )

(use-package org-roam-dailies
  :load-path "load-lisp/org-roam/extensions"
  :after (org emacsql)
  :defer 2
  :general
  (general-define-key
   :states '(normal visual emacs)
   :keymaps 'override
   :prefix "SPC d"
   "j" 'org-roam-dailies-capture-today
   "k" 'org-roam-dailies-goto-today
   "n" 'org-roam-dailies-goto-next-note
   "p" 'org-roam-dailies-goto-previous-note
   "t" 'org-roam-dailies-capture-today
   ))

(use-package org-roam-export
  :load-path "load-lisp/org-roam/extensions"
  :defer t)

(use-package org-roam-graph
  :load-path "load-lisp/org-roam/extensions"
  :defer t)

(use-package org-roam-overlay
  :load-path "load-lisp/org-roam/extensions"
  :defer t)

(use-package org-roam-protocol
  :load-path "load-lisp/org-roam/extensions"
  :defer t)
;; deft
(use-package deft
  :general
  (general-define-key
   :states '(normal visual emacs)
   :keymaps 'override
   :prefix "SPC n"
   "d" 'deft
   )
  :custom
  (deft-recursive t)
  (deft-use-filter-string-for-filename t)
  (deft-default-extension "org")
  (deft-directory org-roam-directory))

(provide 'init-roam)
;;; init-roam.el ends here
