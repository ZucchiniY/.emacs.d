(use-package org
  :commands org-try-structure-completion
  :functions hydra-org-template/body
  :mode ("\\.\\(org\\|org_archive\\)$" . org-mode)
  :hook (org-indent-mode . (lambda() (diminish 'org-indent-mode)))
  :bind (("C-c a" . org-agenda)
         ("C-c o" . org-capture)
         ("C-c b" . org-switchb))
  :config
  (setq org-agenda-files '("~/workspace/org/gtd/")
        org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "HANGUP(h)"
                                      "|" "DONE(d)" "CANCEL(c)"))
        org-log-done 'time
        org-startup-indented t
        org-pretty-entities t
        ;; 不经意的编辑了一些不可见内容的时候，可以帮助我们发现这些编辑的内容
        org-hide-emphasis-markers nil        
        org-catch-invisible-edits 'smart
        org-log-into-drawer 'LOGBOOK
        org-agenda-text-search-extra-files 'agenda-archives
        org-agenda-skip-deadline-if-done t)
  
  (add-hook 'org-mode-hook 'toggle-truncate-lines)

  ;; configurations org keywords' faces
  (setq org-todo-keyword-faces `(("TODO" . (:foreground "SpringGreen2" :weight bold))
                                 ("NEXT" . (:foreground "yellow2" :weight bold))
                                 ("HANGUP" . (:foreground "MediumPurple2" :weight bold :underline t))
                                 ("DONE" . (:foreground "ForestGreen"))
                                 ("CANCEL" . (:foreground "DarkGrey" :underline t))
                                 ("MEETING" . ((:foreground "LightSeaGreen" :weight bold)))))

  ;; org capture-templates
  (setq org-capture-templates
        '(("t" "Todo" entry (file+headline "~/workspace/org/gtd/tasks.org" "Tasks")
           "* TODO %^{Title} %^g\n SCHEDULED: %^T"
           :empty-lines 1)
          ("m" "Meeting" entry (file+headline "~/workspace/org/gtd/tasks.org" "Tasks")
           "* TODO %^{Title} %^g\n  SCHEDULED: %^U\n  %?\n"
           :empty-lines 1)
          ("j" "Journal" entry (file+olp+datetree "~/workspace/org/journal/life.org")
           "* %?\nEntered on %U\n %i\n")
          ("d" "读书笔记" entry (file+datetree "~/workspace/org/journal/reading.org")
           "* 书名: %?\n时间： %U\n** 摘抄: \n** 体会: \n  %i\n "
           :empty-lines 1)))

  ;; More fancy UI
  (use-package org-bullets
    :hook (org-mode . org-bullets-mode)
    :config (setq org-bullets-bullet-list '("☯" "☢" "♠" "♣" "♥" "♦")))

  (use-package org-fancy-priorities
    :diminish
    :defines org-fnacy-priorities-list
    :hook (org-mode . org-fancy-priorities-mode)
    :config
    (setq org-fancy-priorities-list '("HIGHT" "MID" "LOW" "OPTIONAL")))
  
  ;; 替换对应的标记
  ;; 该段正则的意思是 “以0个或者多个空格开头，紧接着一个 ‘-’ ，紧接着是一个空格”
  ;; 将配置上面的情况的 “-” 替换为 “•”
  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))
  ;; Babel
  (setq org-confirm-babel-evaluate nil
        org-src-fontify-natively t
        org-src-tab-acts-natively t)

  (defvar load-language-list '((emacs-lisp . t)
                               (perl . t)
                               (python . t)
                               (ruby . t)
                               (js . t)
                               (css . pt)
                               (sass . t)
                               (shell . t)
                               (C . t)
                               (java . t)
                               (plantuml . t)))

  (use-package ob-go
    :init (cl-pushnew '(go . t) load-language-list))

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
    :bind ("C-c p" . org-pomodoro)
    :config (setq org-pomodoro-long-break-length 15))

  )

;; deft
(use-package deft
  :defer t
  :commands (deft)
  :config
  (setq deft-directory "~/workspace/org"
        deft-recursive t
        deft-extensions '("md" "org")))
;; ox-hugo to help us write blog with org mode and publish with markdown
(use-package ox-hugo
  :after ox)

(with-eval-after-load 'org-capture
  (defun org-hugo-new-subtree-post-capture-template ()
    "Return `org-capture' template string for new Hugo post."
    (let* ((date (format-time-string (org-time-stamp-format :long :inactive) (org-current-time)))
           (title (read-from-minibuffer "Post Title: ")) ;Prompt to enter the post title
           (file-name (read-from-minibuffer "File Name: "))
           (fname (org-hugo-slug file-name)))
      (mapconcat #'identity
                 `(
                   ,(concat "* TODO " title "     :@??:")
                   ":PROPERTIES:"
                   ,(concat ":EXPORT_FILE_NAME: " fname)
                   ,(concat ":EXPORT_DATE: " date)
                   ":END:"
                   "%?\n")
                 "\n")))

  (add-to-list 'org-capture-templates
               '("h"
                 "Hugo post"
                 entry
                 (file+olp "~/workspace/org/blog/hugo-posts.org" "Blog Ideas")
                 (function org-hugo-new-subtree-post-capture-template))))

(provide 'dylan-org)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-org.el ends here
