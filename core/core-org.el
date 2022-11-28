;;; core-org.el --- summary -*- lexical-binding: t -*-

;; Author: Dylan Yang
;; Maintainer: Dylan Yang
;; Version: 1.1.0
;; Package-Requires: ()
;; Homepage: https://github.com/zucchiniy/.emacs.d/

;;; Commentary:

;; org configurations
;; commentary

;;; Code:
(use-package org
  :defines (org-capture-templates
            org-plantuml-jar-path
            org-ditaa-jar-path)
  :commands org-try-structure-completion
  :mode ("\\.\\(org\\|org_archive\\)$" . org-mode)
  :hook (org-indent-mode . (lambda() (diminish 'org-indent-mode)))
  :ensure org-contrib
  :after evil
  :pin gnu
  :general
  (general-define-key
   :states 'normal
   :keymaps 'override
   :prefix "SPC o"
   "a" 'org-agenda
   "b" 'org-switchb
   "c" 'org-capture
   "e" 'org-deadline
   "g" 'counsel-org-clock-goto
   "h" 'counsel-org-clock-history
   "i" 'org-insert-subheading
   "o" 'org-set-tags-command
   "p" 'org-pomodoro
   ;; "r" 'org-refile
   "s" 'org-schedule
   "t" 'org-todo
   "x" 'counsel-org-clock-context
   "E" 'org-export-dispatch
   "R" 'org-clock-report
   "C-o" 'org-open-at-point
   ;; clock start and stop
   "." 'org-clock-in
   "," 'org-clock-out
   ;; "$" 'org-archive-subtree
   "&" 'org-mark-ring-goto
   "!" 'org-time-stamp-inactive
   "`" 'org-time-stamp
   )
  :config
  (setq org-directory "~/workspace/org"
        org-agenda-files (directory-files-recursively
                          (expand-file-name "roam/daily/" org-directory) "\\.org$")
        org-log-done 'time
        org-startup-indented t
        ;; org-startup-folded 文档默认只显示最顶层
        org-startup-folded 'show2levels
        org-pretty-entities t
        ;; 不经意的编辑了一些不可见内容的时候，可以帮助我们发现这些编辑的内容
        ;; org-hide-emphasis-markers t => 不显示相关的标示符号，显示经过优化的样式
        org-hide-emphasis-markers nil
        org-catch-invisible-edits 'smart
        org-agenda-text-search-extra-files nil ;'agenda-archives
        org-agenda-skip-scheduled-if-done nil
        org-plantuml-jar-path (expand-file-name "extends/plantuml.jar" user-emacs-directory)
        org-ditaa-jar-path (expand-file-name "extends/ditaa0_9.jar" user-emacs-directory)
        ;; `^' 和 `_' 是否转义，如果是 t 就转，nil 不转，{} 就 a_{a} 才转
        org-use-sub-superscripts '{}
        org-log-into-drawer 'LOGBOOK
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
        )
  ;; 配置 clock table 中的 block 选项
        ;; 扩展 org-clock-clocktable-default-properties 参数
  (setq org-clock-clocktable-default-properties '(:scope agenda :maxlevel 1 :compact t :formula % :hidefiles t :fileskip0 t :tags t))
  (plist-put org-clock-clocktable-default-properties :block (format-time-string "%Y-W%V"))
  ;; 增加自动变成完成状态
  (defun org-summary-todo (n-done n-not-done)
    "Switch entry to DONE when all subentries are done, to TODO otherwise."
    (let (org-log-done org-log-states)   ; turn off logging
      (org-todo (if (= n-not-done 0) "DONE" "TODO"))))
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
  
  (add-hook 'org-mode-hook 'toggle-truncate-lines)

  ;; configurations org keywords' name and faces
  (setq org-todo-keywords '(;; Baseline sequence
                            (sequence "TODO(t)" "WAITING(w)" "IMPORTANT(i)"
                                      "|" "DONE(d!)" "CANCELED(c@)"))
        org-todo-keyword-faces '(("TODO" . (:foreground "SpringGreen2" :weight bold))
                                 ("CANCELED" . (:foreground "white" ))
                                 ("IMPORTANT" . "orange red")
                                 ("WAITING" . "chocolate")
                                 ("DONE" . "ForestGreen")
                                 ))

  ;; org capture-templates
  (setq org-capture-templates
        '(
          ("r" "Reading" entry
           (file+headline "~/workspace/org/tasks.org" "Reading")
           "* TODO %^{book name}\n%t\n"
           :clock-in t
           :clock-resume t
           :empty-lines 1)
          ("e" "Emotion Notes" entry
           (file+olp+datetree "~/workspace/org/roam/daily/Emotion.org")
           "* %?\nEntered on %U\n %i\n"
           :empty-lines 1
           :jump-to-captured t)
           ))

  ;; More fancy UI
  (use-package org-bullets
    :hook (org-mode . org-bullets-mode)
    :config (setq org-bullets-bullet-list '("☯" "☢" "♠" "♣" "♥" "♦")))

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
                               (python . t)
                               (js . t)
                               (css . t)
                               (sass . t)
                               (shell . t)
                               (dot . t)
                               (ditaa .t)
                               (plantuml . t)
                               
                               ))

  (use-package ob-ipython
    :if (executable-find "jupyter")     ; DO NOT remove
    :init (cl-pushnew '(ipython . t) load-language-list))

  (org-babel-do-load-languages 'org-babel-load-languages
                               load-language-list)

  ;; Preview
  (use-package org-preview-html
    :diminish org-preview-html-mode)

  ;; Pomodoro
  (use-package org-pomodoro
    :after org-agenda
    :config (setq org-pomodoro-long-break-length 15)))

(use-package plantuml-mode
  :mode ("\\.plantuml\\'" . plantuml-mode)
  :defer t
  :config
  (setq plantuml-jar-path (expand-file-name "extends/plantuml.jar" user-emacs-directory)
        plantuml-default-exec-mode 'jar))

(use-package yaml-mode
  :mode (("\\.yaml\\'" . yaml-mode)
         ("\\.yml\\'" . yaml-mode)))

(use-package toml-mode
  :mode (("\\.toml\\'" . toml-mode)))

(use-package org-preview-html
  :config
  (setq org-preview-html-refresh-configuration 'manual
        org-preview-html-viewer 'xwidget))

(provide 'core-org)
;;; core-org.el ends here
