;; init-custom.el --- Define customizations.	-*- lexical-binding: t -*-

;; Author: Dylan Yang
;; URL: https://github.com/zucchiniy/.emacs.d

;;; Commentary:
;;
;; Minimal customizations that are actually used.
;;

;;; Code:

(defcustom dylan-icon t
  "Display icons or not."
  :group 'dylan
  :type 'boolean)

(defcustom dylan-tree-sitter t
  "Enable tree-sitter or not."
  :group 'dylan
  :type 'boolean)

(defcustom dylan-auto-themes '(("8:00"  . ef-elea-light)
                                ("19:00" . ef-elea-dark))
  "List of themes mapped to the time they should be loaded."
  :group 'dylan
  :type '(alist :key-type (string :tag "Time")
                :value-type (symbol :tag "Theme")))

(provide 'init-custom)
;;; init-custom.el ends here
