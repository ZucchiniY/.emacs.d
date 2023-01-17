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

;; recyle: 将 org-id 相关配置迁移到 core-rog.el 配置下

;;; Code:
(require 'core-org)
(require 'org-id)

(use-package org-roam
  :load-path "site-lisp/org-roam"
  :defines (org-roam-dailies-directory
            org-roam-dailies-capture-templates
            org-roam-capture-ref-templates)
  :defer 1
  :ensure t
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
   )
  :config
  (setq org-roam-directory (expand-file-name (concat org-directory "/roam"))
        org-roam-db-gc-threshold most-positive-fixnum
        org-roam-completion-everywhere t
        org-roam-dailies-directory "daily/"
        org-roam-dailies-capture-templates
        '(
          ("w" "Weekly Journal" entry
           "** TODO %^{Title} %^G\nSCHEDULED: %^T %?"
           :target (file+head+olp "%<%Y-W%W>.org"
                                  "#+title: %<%Y 年第 %W 周>\n"
                                  ("关键任务"))
           :empty-lines 1
           :unnarrowed t
           :jump-to-captured t)
          ("m" "Monthly Journal")
          ("mk" "Monthly Journal key result" entry
           "* TODO %^{Title} %^G\nSCHEDULED: %^T %?"
           :target (file+head+olp "%<%Y-%m>.org"
                                  "#+title: %<%Y 年 %m 月>\n"
                                  ("关键任务"))
           :empty-lines 1
           :unnarrowed t)
          ("mr" "Monthly Journal reading" table-line
           "| %? |"
           :target (file+head+olp "%<%Y-%m>.org"
                              "#+title: %<%Y 年 %m 月>\n"
                              ("阅读计划"))
           :empty-lines 1
           :unnarrowed t)
          ("j" "Quarter Journal" entry
           "* TODO %^{Title} %^G\nSCHEDULED: %^T %?"
           :target (file+head "%<%Y-Q%q>.org"
                              "#+title: %<%Y 年 %q 季度>\n")
           :empty-lines 1
           :unnarrowed t
           :jump-to-captured t)
          )
        org-roam-capture-templates
        '(("d" "default" plain "%?"
           :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                              "#+title: ${title}")
           :empty-lines 1
           :unnarrowed t
           :jump-to-captured t)
          ("r" "Reading List" entry
           "* %?"
           :target (file+head "%<%^{Year} 年>阅读清单.org"
                              "#+title: %<%^{Year} 年>阅读清单\n")
           :empty-lines 1
           :unnarrowed t
           :jump-to-capture t)
          )
        )
  (setq org-roam-node-display-template
        (concat "${title:*} "
                (propertize "${tags:10}" 'face 'org-tag)))
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
  :bind ("C-c n d" . deft)
  :custom
  (deft-recursive t)
  (deft-use-filter-string-for-filename t)
  (deft-default-extension "org")
  (deft-directory org-roam-directory))

(provide 'core-roam)
;;; core-roam.el ends here
