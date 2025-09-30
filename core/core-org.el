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
  :ensure org
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
   "r" 'org-refile
   "s" 'org-schedule
   "t" 'org-todo
   "x" 'counsel-org-clock-context
   "A" 'org-archive-subtree
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
                          (expand-file-name "roam/projects/" org-directory) "\\.org$")
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
        ;; 启用 #+bind:
        org-export-allow-bind-keywords t
        ;; 设置归档位置
        org-archive-location (format-time-string "%Y.org::datetree/")
        ;; refile 使用路径
        org-refile-use-outline-path 'file
        org-outline-path-complete-in-steps t
        )
  ;; 配置 clock table 中的 block 选项
  ;; 扩展 org-clock-clocktable-default-properties 参数
  (setq org-clock-clocktable-default-properties '(:scope agenda :maxlevel 2 :compact t :formula % :hidefiles t :fileskip0 t :tags t))
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
        ;; org-todo-keyword-faces '(("TODO" . (:foreground "SpringGreen2" :weight bold))
        ;;                          ("CANCELED" . (:foreground "#354863" ))
        ;;                          ("IMPORTANT" . "orange red")
        ;;                          ("WAITING" . "chocolate")
        ;;                          ("DONE" . "ForestGreen")
        ;;                          )
        )

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

  ;; org-refile-targets 指定移动的文件
  ;; org-refile-use-outlinne-path 'file 显示文件路径
  ;; org-outline-path-complete-in-steps t 逐步选择目标位置
  (setq org-refile-targets '((nil :maxlevel . 5)
                             (org-agenda-files :maxlevel . 5))
        org-refile-use-outline-path 'file
        org-outline-path-complete-in-steps t)

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
                               (mermaid . t)
                               (rust . t)
                               ))

  (use-package ob-rust
    :load-path "load-lisp/ob-rust"
    :init (cl-pushnew '(rust . t) load-language-list))

  ;; (use-package ob-ipython
  ;;   :if (executable-find "jupyter")     ; DO NOT remove
  ;;   :init (cl-pushnew '(ipython . t) load-language-list))

  ;; 新增 mermaid 配置
  (use-package ob-mermaid
    :config
    (setq ob-mermaid-cli-path "/Users/dylan/.nvm/versions/node/v19.8.1/bin/mmdc")
    )

  (org-babel-do-load-languages 'org-babel-load-languages
                               load-language-list)

  ;; org latex process
  (setq org-latex-pdf-process '("xelatex -interaction nonstopmode %f"
                                "xelatex -interaction nonstopmode %f"))

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

(use-package mermaid-mode
  :mode (("\\.mermaid\\'" . mermaid-mode)))

(use-package org-preview-html
  :config
  (setq org-preview-html-refresh-configuration 'manual
        org-preview-html-viewer 'xwidget))

(use-package org-fragtog
  :after org
  :hook (org-mode . org-fragtog-mode))

(use-package org-crypt
  :ensure nil
  :after org
  :config
  (org-crypt-use-before-save-magic)
  (setq org-crypt-key "37ABDC94D3919CB677D1A9FE57F3F2CE5B8B53CA"
        org-crypt-disable-auto-save t
        org-tags-exclude-from-inheritance '("crypt"))
  :bind
  (:map org-mode-map
        ("C-c e" . org-encrypt-entry)
        ("C-c E" . org-encrypt-entries)
        ("C-c d" . org-decrypt-entry)
        ("C-c D" . org-decrypt-entries)))

(provide 'core-org)
;;; core-org.el ends here
