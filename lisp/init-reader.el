;; init-reader.el --- Initialize readers.	-*- lexical-binding: t -*-

;; Copyright (C) 2019-2026 Dylan Yang

;; Author: Dylan Yang <banshiliuli1990@sina.com>
;; URL: https://github.com/zucchiniy/.emacs.d

;;; Commentary:
;;
;; EPUB/RSS readers.
;;

;;; Code:
;; (eval-when-compile
;;   (require 'init-const))

;; Atom/RSS reader
(use-package elfeed
  :pretty-hydra
  ((:title (pretty-hydra-title "Elfeed" 'faicon "nf-fa-rss_square" :face 'nerd-icons-orange)
           :color amaranth :quit-key ("q" "C-g"))
   ("Search"
    (("c" elfeed-db-compact "compact db")
     ("g" elfeed-search-update--force "refresh")
     ("G" elfeed-search-fetch "update")
     ("y" elfeed-search-yank "copy URL")
     ("+" elfeed-search-tag-all "tag all")
     ("-" elfeed-search-untag-all "untag all")
     ("<" elfeed-search-first-entry "first entry")
     (">" elfeed-search-last-entry "last entry"))
    "Filter"
    (("l" elfeed-search-live-filter "live filter")
     ("s" elfeed-search-set-filter "set filter")
     ("*" (elfeed-search-set-filter "@6-months-ago +star") "starred")
     ("a" (elfeed-search-set-filter "@6-months-ago") "all")
     ("t" (elfeed-search-set-filter "@1-day-ago") "today"))
    "Article"
    (("b" elfeed-search-browse-url "browse")
     ("n" next-line "next")
     ("p" previous-line "previous")
     ("u" elfeed-search-tag-all-unread "mark unread")
     ("r" elfeed-search-untag-all-unread "mark read")
     ("RET" elfeed-search-show-entry "show"))))
  :bind (("C-x j" . elfeed)
         :map elfeed-search-mode-map
         ("h" . elfeed-hydra/body)
         ("?" . elfeed-hydra/body))
  :init (setq url-queue-timeout 30
              elfeed-db-directory (locate-user-emacs-file ".elfeed")
              elfeed-show-entry-switch #'pop-to-buffer
              elfeed-show-entry-delete #'delete-window
              elfeed-feeds '(("https://planet.emacslife.com/atom.xml" planet emacslife)
                             ("http://www.masteringemacs.org/feed/" mastering)
                             ("https://oremacs.com/atom.xml" oremacs)
                             ("https://pinecast.com/feed/emacscast" emacscast)
                             ("https://emacstil.com/feed.xml" Emacs TIL)
                             ;; ("https://www.reddit.com/r/emacs.rss" reddit)
                             ))
  :config
  ;; Ignore db directory in recentf
  (push elfeed-db-directory recentf-exclude)

  ;; Add icons via tags
  (when (icons-displayable-p)
    (defun nerd-icon-for-tags (tags)
      "Generate Nerd Font icon based on tags.
  Returns default if no match."
      (cond ((member "youtube" tags)  (nerd-icons-faicon "nf-fa-youtube_play" :face '(:foreground "#FF0200")))
            ((member "instagram" tags) (nerd-icons-faicon "nf-fa-instagram" :face '(:foreground "#FF00B9")))
            ((or (member "emacs" tags) (member "emacslife" tags) (member "mastering" tags))
             (nerd-icons-sucicon "nf-custom-emacs" :face '(:foreground "#9A5BBE")))
            ((member "github" tags) (nerd-icons-faicon "nf-fa-github"))
            (t (nerd-icons-faicon "nf-fae-feedly" :face '(:foreground "#2AB24C")))))

    (defun my-elfeed-search-print-entry (entry)
      "Print ENTRY to the buffer."
      (let* ((date (elfeed-search-format-date (elfeed-entry-date entry)))
             (date-width (car (cdr elfeed-search-date-format)))
             (title (concat (or (elfeed-meta entry :title)
                                (elfeed-entry-title entry) "")
                            ;; NOTE: insert " " for overlay to swallow
                            " "))
             (title-faces (elfeed-search--faces (elfeed-entry-tags entry)))
             (feed (elfeed-entry-feed entry))
             (feed-title (when feed (or (elfeed-meta feed :title) (elfeed-feed-title feed))))
             (tags (mapcar #'symbol-name (elfeed-entry-tags entry)))
             (tags-str (mapconcat (lambda (s) (propertize s 'face 'elfeed-search-tag-face)) tags ","))
             (title-width (- (frame-width)
                             ;; (window-width (get-buffer-window (elfeed-search-buffer) t))
                             date-width elfeed-search-trailing-width))
             (title-column (elfeed-format-column
                            title (elfeed-clamp
                                   elfeed-search-title-min-width
                                   title-width
                                   elfeed-search-title-max-width) :left))

             ;; Title/Feed ALIGNMENT
             (align-to-feed-pixel (+ date-width
                                     (max elfeed-search-title-min-width
                                          (min title-width elfeed-search-title-max-width)))))
        (insert (propertize date 'face 'elfeed-search-date-face) " ")
        (insert (propertize title-column 'face title-faces 'kbd-help title))
        (put-text-property (1- (point)) (point) 'display `(space :align-to ,align-to-feed-pixel))
        (when feed-title
          (insert " " (concat (nerd-icon-for-tags tags) " ")
                  (propertize feed-title 'face 'elfeed-search-feed-face) " "))
        (when tags (insert "(" tags-str ")"))))

    (setq  elfeed-search-print-entry-function #'my-elfeed-search-print-entry)))

(provide 'init-reader)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-reader.el ends here
