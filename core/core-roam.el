;;; core-roam.el --- summary -*- lexical-binding: t -*-

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

;; recyle: 将 org-id 相关配置迁移到 core-org.el 配置下

;;; Code:
(require 'core-org)
(require 'org-id)

(use-package org-roam
  :load-path "site-lisp/org-roam"
  :defines (org-roam-dailies-directory
            org-roam-dailies-capture-templates
            org-roam-capture-ref-templates)
  :defer 1
  :diminish org-roam-mode
  :general
  (general-define-key
   :states '(normal visual)
   :keymaps 'override
   :prefix "SPC n"
   "a" 'org-roam-alias-add
   "c" 'org-roam-capture
   "f" 'org-roam-node-find
   "g" 'org-id-get-create
   "i" 'org-roam-node-insert
   "j" 'org-roam-dailies-capture-today
   "k" 'org-roam-dailies-goto-today
   "l" 'org-roam-buffer-toggle
   "n" 'org-roam-dailies-goto-next-note
   "p" 'org-roam-dailies-goto-previous-note
   "r" 'org-roam-ref-add
   "s" 'org-roam-db-sync
   "t" 'org-roam-tag-add
   "u" 'org-roam-ui-mode
   "U" 'org-id-update-id-locations
   )
  :config
  ;; 解决 org-roam-ui 仅显示一个 Tag 问题
  (setq org-roam-database-connector 'sqlite-builtin)
  (setq org-roam-directory (expand-file-name (concat org-directory "/roam"))
        org-roam-db-gc-threshold most-positive-fixnum
        org-roam-completion-everywhere t
        org-roam-dailies-directory "projects/"
        org-roam-dailies-capture-templates
        '(("t" "临时任务" entry
           "* TODO [#B] %^{Title} %^G\nSCHEDULED: %^T %?"
           :target (file+head "tasks.org"
                              "#+title: 收集箱")
           :empty-lines 1
           :jump-to-capture t
           :unnarrowed t)
          ("p" "项目" entry
           "* DOING [#B] %^{Title} %^G\nDEADLINE: %^T SCHEDULED: %^T\n%?"
           :target (file+head "projects.org"
                              "#+title: 项目清单")
           :empty-lines 1
           :jump-to-capture t
           :unnarrowed t)
          )
        org-roam-capture-templates
        '(("a" "领域")
          ("ad" "开发" plain
           "#+filetags: %^{Tags?|database|docker|ML|language|agility}"
           :target (file+head "areas/develop/%^{Tags?|database|docker|ML|language|agility}/%<%Y%m%d%H%M%S>-${slug}.org"
                              "#+title: ${title}")
           :empty-lines 1
           :prepend t
           :jump-to-captured t
           :unnarrowed t
           )
          ("at" "工具相关" plain
           "#+filetags: %^{Tags|emacs|systems}"
           :target (file+head "areas/tools/%^{Tags|emacs|systems}/%<%Y%m%d%H%M%S>-${slug}.org"
                              "#+title: ${title}")
           :empty-lines 1
           :prepend t
           :jump-to-captured t
           :unnarrowed t
           )
          ("af" "理财" plain
           "#+filetags: %^{Tags|strategy|notes}"
           :target (file+head "areas/financial/%^{Tags|strategy|notes}/%<%Y%m%d%H%M%S>-${slug}.org"
                              "#+title: ${title}")
           :empty-lines 1
           :prepend t
           :jump-to-captured t
           :unnarrowed t
           )
          ("aw" "写作")
          ("awn" "笔记" plain
           "#+filetags: :novel:notes:"
           :target (file+head "areas/writing/notes/%<%Y%m%d%H%M%S>-${slug}.org"
                              "#+title: ${title}")
           :empty-lines 1
           :prepend t
           :jump-to-captured t
           :unnarrowed t
           )
          ("awm" "素材" plain
           "#+filetags: :novel:material:"
           :target (file+head "areas/writing/material/%<%Y%m%d%H%M%S>-${slug}.org"
                              "#+title: ${title}")
           :empty-lines 1
           :prepend t
           :jump-to-captured t
           :unnarrowed t
           )
          ("aws" "小说" table-line
           "| %? | | |"
           :target (file+head+olp "areas/writing/novel/${slug}.org"
                                  "#+title: ${title}"
                                  ("章节"))
           :jump-to-captured t
           :unnarrowed t
           )
          ("ar" "阅读")
          ("arn" "阅读笔记" plain
           "#+filetags: :reading:notes:%^{Tags?|book|blog}"
           :target (file+head "areas/reading/%<%Y%m%d%H%M%S>-${slug}.org"
                              "#+title: ${title}")
           :empty-lines 1
           :prepend t
           :jump-to-captured t
           :unnarrowed t
           )
          ("arl" "阅读清单" table-line
           "| %? |"
           :target (file+head+olp "areas/reading/%<%^{Year}年>阅读清单.org"
                                  "#+title: %<%^{Year}年>阅读清单\n"
                                  ("读书"))
           :jump-to-captured t
           :unnarrowed t
           )
          ("r" "资源" plain
           "#+filetags: :resources:notes:%^{Tags?|golang|javascript|rust}"
           :target (file+head "resources/%<%Y%m%d%H%M%S>-${slug}.org"
                              "#+title: ${title}")
           :empty-lines 1
           :prepend t
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

  (require 'org-roam-protocol))

;; org-roam-ui
(use-package org-roam-ui
  :after org-roam
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t))

;; deft
(use-package deft
  ;; :bind ("C-c n d" . deft)
  :general
  (general-define-key
   :states '(normal visual)
   :keymaps 'override
   :prefix "SPC n"
   "d" 'deft
   )
  :custom
  (deft-recursive t)
  (deft-use-filter-string-for-filename t)
  (deft-default-extension "org")
  (deft-directory org-roam-directory))

(provide 'core-roam)
;;; core-roam.el ends here
