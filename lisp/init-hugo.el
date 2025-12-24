;; init-hugo.el --- Use org to write blog.	-*- lexical-binding: t -*-

;; Copyright (C) 2019-2021 Dylan Yang

;; Author: Dylan Yang <banshiliuli1990@sina.com>
;; URL: https://github.com/zucchiniy/.emacs.d

;;; Commentary:
;;

;;; Code:
(use-package ox-hugo
  :ensure t
  :defer 1
  :init
  (with-eval-after-load 'org-capture
    (defun org-hugo-new-subtree-post-capture-template ()
      "Return `org-capture' template string for new Hugo post."
      (let* ((date (format-time-string (org-time-stamp-format :long :inactive) (org-current-time)))
             (title (read-from-minibuffer "Post Title: "))
             (file-name (read-from-minibuffer "File Name: "))
             (fname (org-hugo-slug file-name)))
        (mapconcat #'identity
                   `(
                     ,(concat "* TODO " title " %^g")
                     ":PROPERTIES:"
                     ,(concat ":EXPORT_FILE_NAME: " fname)
                     ,(concat ":EXPORT_DATE: " date)
                     ,(concat ":EXPORT_HUGO_MENU: ")
                     ,(concat ":EXPORT_HUGO_CUSTOM_FRONT_MATTER: ")
                     ":END:"
                     "%?\n")
                   "\n")))
    (add-to-list 'org-capture-templates '("h" "Hugo Blog Post"))
    (add-to-list 'org-capture-templates
                 '("hi"
                   "Investment in Hugo post"
                   entry
                   (file "~/workspace/blog/content-org/invest-posts.org")
                   (function org-hugo-new-subtree-post-capture-template)))
    (add-to-list 'org-capture-templates
                 '("ht"
                   "Technology in Hugo post"
                   entry
                   (file "~/workspace/blog/content-org/tech-posts.org")
                   (function org-hugo-new-subtree-post-capture-template)))
    )
  )
(provide 'init-hugo)
;;; init-hugo.el ends here
