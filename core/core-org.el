(use-package org
  :defines org-capture-templates org-plantuml-jar-path org-ditaa-jar-path
  :commands org-try-structure-completion
  :mode ("\\.\\(org\\|org_archive\\)$" . org-mode)
  :hook (org-indent-mode . (lambda() (diminish 'org-indent-mode)))
  :general
  (general-define-key
   :states 'normal
   :keymaps 'override
   :prefix "SPC o"
   "a" 'org-agenda
   "b" 'org-switchb
   "c" 'org-capture
   "t" 'org-todo
   "o" 'org-set-tags
   "g" 'org-clock-goto
   "." 'org-clock-in
   "," 'org-clock-out
   "x" 'counsel-org-clock-context
   "h" 'counsel-org-clock-history
   "R" 'org-clock-report
   "E" 'org-export-dispatch
   "r" 'org-refile
   "$" 'org-archive-subtree
   "s" 'org-schedule
   "e" 'org-deadline)
  :config
  (setq org-agenda-files '("~/workspace/gtd/")
        org-log-done 'time
        org-startup-indented t
        org-pretty-entities t
        ;; 不经意的编辑了一些不可见内容的时候，可以帮助我们发现这些编辑的内容
        org-hide-emphasis-markers nil
        org-catch-invisible-edits 'smart
        org-agenda-text-search-extra-files 'agenda-archives
        org-agenda-skip-scheduled-if-done t
        org-plantuml-jar-path (expand-file-name "plantuml.jar" user-emacs-directory)
        org-ditaa-jar-path (expand-file-name "ditaa0_9.jar" user-emacs-directory)
        ;; `^' 和 `_' 是否转义，如果是 t 就转，nil 不转，{} 就 a_{a} 才转
        org-use-sub-superscripts '{}
        org-log-into-drawer 'LOGBOOK
        org-agenda-skip-deadline-if-done t
        org-descriptive-links nil)
  ;; 加载一些 org modules
  (setq org-modules '(org-habit))
  
  (add-hook 'org-mode-hook 'toggle-truncate-lines)

  ;; configurations org keywords' name and faces
  (setq org-todo-keywords '(;; Baseline sequence
                            (sequence "☞ TODO(t)" "✰ Important(i)" "⚑ WAITING(w)"
                                      "|" "✔ DONE(d!)" "✘ CANCELED(c@)" "⚔ STARTED(s)")
                            ;; Note information
                            (sequence "|" "✍ NOTE(N)" "☕ BREAK(b)" "FIXME"))
        org-todo-keyword-faces '(("☞ TODO" . (:foreground "SpringGreen2" :weight bold))
                                 ("⚔ STARTED"  . "gold")
                                 ("✘ CANCELED" . (:foreground "white" :background "DarkGrey" :weight bold))
                                 ("⚑ WAITING" . "chocolate")
                                 ("✔ DONE" . "ForestGreen")
                                 ("FIXME" . "firebrick")))

  ;; org capture-templates
  (setq org-capture-templates
        '(("t" "Todo" entry (file+headline "~/workspace/gtd/tasks.org" "Tasks")
           "* ☞ TODO %^{Title} %^g\n SCHEDULED: %^T"
           :empty-lines 1)
          ("b" "Habits" entry (file+headline "~/workspace/gtd/tasks.org" "Habits")
           "* ☞ TODO %^{Title} %^g\n SCHEDULED: %^T\n :PROPERTIES:\n :STYLE: habit\n :END:"
           :empty-lines 1)
          ("l" "Learning" entry (file+olp+datetree "~/workspace/journal/learning.org")
           "* %?\nEntered on %U\n %i\n"
           :empty-lines 1)
          ("j" "Journal" entry (file+olp+datetree "~/workspace/journal/journal.org")
           "* %?\nEntered on %U\n %i\n"
           :empty-lines 1)))

  ;; More fancy UI
  (use-package org-bullets
    :hook (org-mode . org-bullets-mode)
    :config (setq org-bullets-bullet-list '("☯" "☢" "♠" "♣" "♥" "♦")))

  (use-package org-fancy-priorities
    :diminish
    :hook (org-mode . org-fancy-priorities-mode)
    :config
    (setq org-fancy-priorities-list '("⚡" "⬆" "⬇" "☕")))
  
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
                               (css . t)
                               (sass . t)
                               (shell . t)
                               (dot . t)
                               (C . t)
                               (java . t)
                               (ditaa .t)
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
    :config (setq org-pomodoro-long-break-length 15)))

;; deft
 (use-package deft
  :defer t
  :commands (deft)
  :config
  (setq deft-directory "~/workspace/"
        deft-recursive t
        deft-extensions '("org" "md")))
;; ox-hugo to help us write blog with org mode and publish with markdown
(use-package ox-hugo
  :after ox)

(with-eval-after-load 'org-capture
  (defun org-hugo-new-subtree-post-capture-template ()
    "Return `org-capture' template string for new Hugo post."
    (let* ((date (format-time-string (org-time-stamp-format :long :inactive) (org-current-time)))
           (title (read-from-minibuffer "Post Title: "))
           (file-name (read-from-minibuffer "File Name: "))
           (fname (org-hugo-slug file-name)))
      (mapconcat #'identity
                 `(
                   ,(concat "* TODO " title)
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
                 (file "~/workspace/blog/hugo-posts.org")
                 (function org-hugo-new-subtree-post-capture-template))))

(provide 'core-org)
