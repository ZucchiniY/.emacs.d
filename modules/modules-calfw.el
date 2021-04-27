;; modules-calfw.el --- Use Calfw GTD.	-*- lexical-binding: t -*-

;; Copyright (C) 2019-2021 Dylan Yang

;; Author: Dylan Yang <banshiliuli1990@sina.com>
;; URL: https://github.com/zucchiniy/.emacs.d

;;; Commentary:
;;

;;; Code:
         
(use-package calfw
  :defer t
  :commands (cfw:open-calendar-buffer))

(use-package calfw-org
  :defer t
  :after (calfw org-agenda)
  :commands (cfw:open-org-calendar cfw:org-create-source)
  :config
  (setq cfw:org-face-agenda-item-foreground-color "#ecccc3"))

(use-package calfw-cal
  :ensure t)

(provide 'modules-calfw)
;;; modules-calfw.el ends here

