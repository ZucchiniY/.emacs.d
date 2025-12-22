;; core-custom.el --- Define basis config.	-*- lexical-binding: t -*-

;; Copyright (C) 2019-2021 Dylan Yang

;; Author: Dylan Yang <banshiliuli1990@sina.com>
;; URL: https://github.com/zucchiniy/.emacs.d

;;; Commentary:
;;
;; Customization
;;
;;; Code:
(defgroup dylan nil
  "Dylan Emacs Customization."
  :group 'convenience
)

(defcustom dylan-full-name user-full-name
  "Set User full name."
  :group 'dylan
  :type 'string)

(defcustom dylan-logo (expand-file-name
                       (if (display-graphic-p) "logo.png" "banner.txt")
                       user-emacs-directory)
  "Set Dylan log.nil means official logo."
  :group 'dylan
  :type 'string)

(provide 'core-custom)
;;; core-custom.el ends here
