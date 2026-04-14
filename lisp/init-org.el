;;; init-org.el --- summary -*- lexical-binding: t -*-

;; Copyright (C) 2019-2026 Dylan Yang

;; Author: Dylan Yang <banshiliuli1990@sina.com>
;; URL: https://github.com/zucchiniy/.emacs.d

;;; Commentary:

;; org configurations

;;; Code:
(eval-when-compile
  (require 'init-general))

(use-package org
  :defines (org-capture-templates
)
  :commands org-try-structure-completion
  :mode ("\\.\\(org\\|org_archive\\)$" . org-mode)
  :hook (org-indent-mode . (lambda() (diminish 'org-indent-mode)))
  :ensure org
  :pin gnu
  :general
  (general-define-key
   :states '(normal visual emacs)
   :keymaps 'override
   :prefix "SPC o"
   "a" 'org-agenda
   "b" 'org-switchb
   "c" 'org-capture
   "e" 'org-deadline
   "g" 'org-clock-goto
   "i" 'org-insert-subheading
   "o" 'org-set-tags-command
   "r" 'org-refile
   "s" 'org-schedule
   "t" 'org-todo
   "A" 'org-archive-subtree
   "E" 'org-export-dispatch
   "R" 'org-clock-report
   "C-o" 'org-open-at-point
   ;; clock start and stop
   "." 'org-clock-in
   "," 'org-clock-out
   "&" 'org-mark-ring-goto
   "!" 'org-time-stamp-inactive
   "`" 'org-time-stamp
   )
  :config
  (setq org-directory "~/workspace/org"
        org-agenda-files (directory-files-recursively
                          (expand-file-name "roam/projects/" org-directory) "\\.org$")
        org-log-done 'time
        org-startup-indented t
        ;; org-startup-folded 文档默认只显示最顶层
        org-startup-folded 'show5levels
        org-pretty-entities t
        ;; 不经意的编辑了一些不可见内容的时候，可以帮助我们发现这些编辑的内容
        ;; org-hide-emphasis-markers t => 不显示相关的标示符号，显示经过优化的样式
        org-hide-emphasis-markers nil
        org-catch-invisible-edits 'smart
        org-agenda-text-search-extra-files nil ;'agenda-archives
        org-agenda-skip-scheduled-if-done nil
        ;; `^' 和 `_' 是否转义，如果是 t 就转，nil 不转，{} 就 a_{a} 才转
        org-use-sub-superscripts '{}
        org-log-into-drawer 'LOGBOOK
        org-clock-into-drawer 'PROPERTIES
        org-agenda-skip-deadline-if-done t
        ;; nil 表示显示完整链接，t 则显示链接名称
        org-link-descriptive t
        org-babel-python-command "python3"
        ;; auto-clockout-timer
        org-clock-auto-clockout-timer 200
        ;; include entries from diary into agenda
        org-agenda-include-diary t
        ;; 调整 org 中图片的大小
        org-image-actual-width nil
        ;; 启用 #+bind:
        org-export-allow-bind-keywords t
        ;; 设置归档位置
        org-archive-location (expand-file-name (format-time-string "%Y.org::datetree/") (expand-file-name "roam/archives" org-directory))
        ;; refile 使用路径
        org-refile-use-outline-path 'file
        org-outline-path-complete-in-steps t
        )
  ;; 配置 clock table 中的 block 选项
  ;; 扩展 org-clock-clocktable-default-properties 参数
  (setq org-clock-clocktable-default-properties '(:scope agenda-with-archives :maxlevel 5 :filetitle t :compact t :formula % :hidefiles t :fileskip0 t :tags t))
  (plist-put org-clock-clocktable-default-properties :block (format-time-string "%Y-W%V"))
  ;; 增加自动变成完成状态
  (defun org-summary-todo (n-done n-not-done)
    "Switch entry to DONE when all subentries are done, to TODO otherwise."
    (let (org-log-done org-log-states)   ; turn off logging
      (org-todo (if (= n-not-done 0) "已完成" "未开始"))))
  (add-hook 'org-after-todo-statistics-hook #'org-summary-todo)
  ;; 加载一些 org modules
  (setq org-modules '(org-habit
                      org-id))

  ;; org-id 相关配置
  (setq org-id-track-globally t)
  (setq org-id-locations-file (expand-file-name (concat org-directory "/roam/.org-id-locations")))
  ;; config org-id-files
  ;; (setq org-id-files (file-expand-wildcards (expand-file-name "roam/*.org" org-directory)))
  (setq org-id-files (directory-files-recursively (expand-file-name "roam/" org-directory) "\\.org$"))

  ;; When the clock is running and Emacs is idle for more than this number of seconds, the clock will be clocked out automatically
  (org-clock-auto-clockout-insinuate)

  (add-hook 'org-mode-hook (lambda () (setq toggle-truncate-lines nil)))

  ;; configurations org keywords' name and faces
  (setq org-todo-keywords
        '((sequence "未开始(p!)" "进行中(t!)" "阻塞中(s!)"
                    "|" "已完成(d!)" "已取消(c@/!)"))
        org-todo-keyword-faces
        '(("未开始" . (:foreground "red" :weight bold))
          ("阻塞中" . (:foreground "red" :weight bold))
          ("进行中" . (:foreground "orange" :weight bold))
          ("已完成" . (:foreground "green" :weight bold))
          ("已取消" . (:background "gray" :foreground "black")))
        org-agenda-time-grid '((daily today require-timed)
                               (600 800 1000 1200 1400 1600 1800 2000 2200 2400)
                               "......" "----------------")
        ;; agenda log
        org-agenda-log-mode-items '(clock) ;; 仅查看 时间
        org-agenda-log-mode-add-notes nil ;; 不添加笔记
        org-agenda-start-with-log-mode "only" ;; 打开时展示日志，与 org-agenda-log-mode-items 配置一致
        )
  (setq org-agenda-custom-commands
        '(("r" "Daily Agenda"
           ((agenda "" ((org-agenda-overriding-header "今日记录")
                        (org-agenda-span 'day)
                        (org-agenda-show-log 'clockcheck)
                        (org-agenda-start-with-log-mode nil)
                        (org-agenda-log-mode-items '(clock))
                        (org-agenda-clockreport-mode nil)))))))

  ;; org capture-templates
  (setq org-capture-templates
        '(
          ("t" "待办清单" entry
           (file "roam/projects/tasks.org")
           "* 未开始 [#B] %^{title} %^G\nSCHEDULED: %^T %?"
           :empty-lines 1
           :jump-to-captured t
           :unnarrowed t)
          ("s" "学习任务" entry
           (file+headline "roam/projects/studies.org" "学习清单")
           "* 未开始 %^{name}\nSCHEDULED: %^t %?"
           :empty-lines 1)
          ("r" "阅读清单" entry
           (file+headline "roam/projects/studies.org" "阅读清单")
           "* 未开始 %^{name}\nSCHEDULED: %^t %?"
           :empty-lines 1)
          ))

  ;; org-refile-targets 指定移动的文件
  ;; org-refile-use-outlinne-path 'file 显示文件路径
  ;; org-outline-path-complete-in-steps t 逐步选择目标位置
  (setq org-refile-targets '((nil :maxlevel . 5)
                             (org-agenda-files :maxlevel . 5))
        org-refile-use-outline-path 'file
        org-outline-path-complete-in-steps t)

  ;; 替换对应的标记
  ;; 该段正则的意思是 “以 0 个或者多个空格开头，紧接着一个 ‘-’ ，紧接着是一个空格”
  ;; 将配置上面的情况的 “-” 替换为 “•”
  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))
  ;; Babel
  (setq org-confirm-babel-evaluate nil
        org-src-fontify-natively t
        org-src-tab-acts-natively t)

  (defvar load-language-list '((emacs-lisp . t)
                               (shell . t)
                               ))


  (org-babel-do-load-languages 'org-babel-load-languages
                               load-language-list)

  )

(use-package org-superstar
  :after org
  :hook (org-mode . org-superstar-mode)
  :init
  (setq org-superstar-headline-bullets-list '("⒈" "⒉" "⒊" "⒋" "⒌")
        org-ellipsis "⋯"))

(provide 'init-org)
;;; init-org.el ends here
