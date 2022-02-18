;;; modules-writer.el --- summary -*- lexical-binding: t -*-

;; Author: Dylan Yang
;; Maintainer: Dylan Yang
;; Version: 1.0.0
;; Package-Requires: ()

;;; Commentary:

;;; Code:

(use-package writeroom-mode
  :init
  (setq writeroom-extra-line-spacing 5
        writeroom-width 0.5)
  (advice-add 'writeroom--calculate-width :before #'redisplay))

(provide 'modules-writer)

;;; modules-writer.el ends here
