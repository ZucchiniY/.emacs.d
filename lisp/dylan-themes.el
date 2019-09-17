;;; dylan-themes.el --- summary -*- lexical-binding: t -*-

;; Author: Dylan Yang
;; Maintainer: Dylan Yang

;;; Commentary:

;;; Code:
(use-package doom-themes
  :init
  (setq doom-themes-enable-bold t 
        doom-themes-enable-italic t
        doom-one-brighter-comments t
        doom-one-padded-modeline t)
  :config
  (load-theme 'doom-one t)
  (doom-themes-visual-bell-config)
  (doom-themes-org-config)
  (doom-themes-treemacs-config))

(use-package base16-theme)
;; (use-package theme-changer
;;   :init
;;   (setq calendar-location-name "Beijing"
;;         calendar-latitude 39.91
;;         calendar-longitude 116.4)
;;   :config
;;   (change-theme 'base16-gruvbox-light-hard 'doom-dracula))

(provide 'dylan-themes)
;;; dylan-themes.el ends here
