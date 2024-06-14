;; modules-json.el --- Use org to write blog.	-*- lexical-binding: t -*-

;; Copyright (C) 2019-2024 Dylan Yang

;; Author: Dylan Yang <banshiliuli1990@sina.com>
;; URL: https://github.com/zucchiniy/.emacs.d

;;; Commentary:
;;

;;; Code:
(use-package json-mode
  :ensure t
  :mode (("\\.json\\'" . json-mode)
         ("\\.jsonl\\'" . json-mode)))

(provide 'modules-json)
;;; modules-json.el ends here
