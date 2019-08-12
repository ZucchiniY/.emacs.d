;;; dylan-themes.el --- summary -*- lexical-binding: t -*-

;; Author: Dylan Yang
;; Maintainer: Dylan Yang

;;; Commentary:

;;; Code:
(use-package base16-theme)

(use-package doom-themes
  :init
  (setq doom-themes-enable-bold nil
        doom-themes-enable-italic nil)
  :config
  (doom-themes-visual-bell-config)
  (doom-themes-treemacs-config)
  (doom-themes-org-config))

;; (use-package theme-changer
;;   :init
;;   (setq calendar-location-name "Beijing"
;;         calendar-latitude 39.91
;;         calendar-longitude 116.4)
;;   :config
;;   (change-theme 'base16-gruvbox-light-hard 'doom-dracula))

(provide 'dylan-themes)
;;; dylan-themes.el ends here
