;; init-calendar.el --- Define calendar.	-*- lexical-binding: t -*-

;; Copyright (C) 2019-2021 Dylan Yang

;; Author: Dylan Yang <banshiliuli1990@sina.com>
;; URL: https://github.com/zucchiniy/.emacs.d

;;; Commentary:
;;

;;; Code:
(use-package cal-china-x
  :after calendar
  :commands cal-china-x-setup
  :init (cal-china-x-setup)
  :config
  ;; Holidays
  (setq calendar-mark-holidays-flag t
        cal-china-x-important-holidays cal-china-x-chinese-holidays
        cal-china-x-general-holidays '((holiday-lunar 7 7 "七夕节")
                                       (holiday-fixed 6 1 "儿童节"))
        holiday-other-holidays '((holiday-fixed 2 14 "情人节")
                                 (holiday-float 5 0 2 "母亲节")
                                 (holiday-float 6 0 3 "父亲节")
                                 (holiday-lunar 1 7 "父亲生日")
                                 (holiday-lunar 3 3 "母亲生日")
                                 (holiday-fixed 10 18 "老婆生日")
                                 (holiday-fixed 4 6 "女儿生日")
                                 (holiday-fixed 5 1 "岳父生日")
                                 (holiday-fixed 6 15 "岳母生日")
                                )
        calendar-holidays (append cal-china-x-important-holidays
                                  cal-china-x-general-holidays
                                  holiday-other-holidays)))

(use-package calfw
  :defer 1
  :commands (cfw:open-calendar-buffer))

(use-package calfw-org
  :defer 1
  :commands (cfw:open-org-calendar cfw:org-create-source)
  :config
  (setq cfw:org-face-agenda-item-foreground-color "#ecccc3"))

(use-package calfw-cal
  :ensure t)

(provide 'init-calendar)
;;; init-calendar.el ends here
