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

;; 配置 org-roam
(use-package org-roam
  ;; :load-path "load-lisp/org-roam"
  :after (org)
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
   "r" 'org-roam-refile
   "s" 'org-roam-db-sync
   "t" 'org-roam-tag-add
   "U" 'org-id-update-id-locations
   )
  :custom
  (org-roam-database-connector 'sqlite-builtin)
  :config
  (setq org-roam-directory (expand-file-name (concat org-directory "/roam"))
        org-roam-db-gc-threshold most-positive-fixnum
        org-roam-completion-everywhere t
        org-roam-dailies-directory "projects/"
        org-roam-dailies-capture-templates
        '(
          ("w" "weekly" entry
           "** %? \nSCHEDULED: %T\n"
           :target (file+head+olp "%<%Y-W%W>.org"
                                  "#+title: %<%Y-W%W>"
                                  ("本周任务"))
           :empty-lines 1
           :jump-to-captured t
           :unnarrowed t)
          ("m" "monthly" entry
           "** %? \nSCHEDULED: %T"
           :target (file+head+olp "%<%Y-%m>.org"
                                  "#+title: %<%Y-%m>"
                                  ("本月任务"))
           :empty-lines 1
           :jump-to-captured t
           :unnarrowed t)
          )
        org-roam-capture-templates
        '(("p" "项目")
          ("po" "OKR" checkitem
           "- [ ] %?"
           :target (file+head "projects/%<%Y>OKR.org"
                              "#+title: %<%Y>年OKR")
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
           "#+create_date: %<%Y-%m-%d %a>\n#+update_date: %<%Y-%m-%d %a>\n#+category:\n#+filetags:%?"
           :target (file+head "areas/%<%Y%m%d%H%M%S>-${slug}.org"
                              "#+title: ${title}")
           :jump-to-captured t
           :unnarrowed t
           )
          ("r" "资源" plain
           "#+create_date: %<%Y-%m-%d %a>\n#+update_date: %<%Y-%m-%d %a>\n#+category:\n#+filetags:%?"
           :target (file+head "resources/%<%Y%m%d%H%M%S>-${slug}.org"
                              "#+title: ${title}")
           :jump-to-captured t
           :unnarrowed t
           )
          )
        )
  (setq org-roam-node-display-template
        (concat "${title:*} "
                (propertize "${tags:50}" 'face 'org-tag)))
  (org-roam-db-autosync-mode)
  )

;; 配置 org-roam-dailies
(use-package org-roam-dailies
  :load-path "load-lisp/org-roam/extensions"
  :after (org)
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

;; 加载必要的 org-roam 扩展
(use-package org-roam-export
  :load-path "load-lisp/org-roam/extensions"
  :defer t)

(use-package org-roam-graph
  :load-path "load-lisp/org-roam/extensions"
  :defer t)

(use-package org-roam-protocol
  :load-path "load-lisp/org-roam/extensions"
  :defer t)

(provide 'init-roam)
;;; init-roam.el ends here
