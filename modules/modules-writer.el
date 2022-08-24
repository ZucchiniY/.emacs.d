;;; modules-writer.el --- summary -*- lexical-binding: t -*-

;; Author: Dylan Yang
;; Maintainer: Dylan Yang
;; Version: 2.0.0
;; Package-Requires: ()

;;; Commentary:
;; feature: 新增 pangu-spacing 包，用来控制空格的显示

;;; Code:

(use-package writeroom-mode
  :init
  (setq writeroom-extra-line-spacing 5
        writeroom-width 0.5)
  (advice-add 'writeroom--calculate-width :before #'redisplay))

;; (use-package pangu-spacing
;;   :init
;;   (global-pangu-spacing-mode t)
;;   (setq pangu-spacing-real-insert-separtor t)
;;   :config
;;   (add-hook 'org-mode-hook
;;             (lambda ()
;;               (set (make-local-variable 'pangu-spacing-real-insert-separtor) t))))

(provide 'modules-writer)

;;; modules-writer.el ends here
