;; core-ui.el --- Define ui config.	-*- lexical-binding: t -*-

;; Copyright (C) 2019-2021 Dylan Yang

;; Author: Dylan Yang <banshiliuli1990@sina.com>
;; URL: https://github.com/zucchiniy/.emacs.d

;;; Commentary:
;;

;;; Code:
;; 隐藏滚动条、菜单栏
(require 'core-basis)

(unless sys/winntp
  (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(when (fboundp 'horizontal-scroll-bar-mode)
  (horizontal-scroll-bar-mode -1))
(when sys/macp
  (setq dired-use-ls-dired nil))
(tooltip-mode nil)
(setq make-backup-files nil)
(winner-mode t)


(cond (sys/mac-x-p (dylan//set-monospaced-font "Iosevka Nerd Font Mono" "华文仿宋" 14 14))
      (sys/linux-x-p (dylan//set-monospaced-font "Iosevka Nerd Font Mono" "Wenquanyi Micro Hei Mono" 18 18))
      (sys/win-x-p (dylan//set-monospaced-font "Iosevka Nerd Font Mono" "Microsoft YaHei" 14 14)))

(use-package doom-modeline
  :ensure t
  :after (nerd-icons)
  :hook (after-init . doom-modeline-mode)
  :config
  (setq doom-modeline-icon t
        doom-modeline-major-mode-color-icon t
        doom-modeline-major-mode-icon t
        doom-modeline-buffer-state-icon t
        doom-modeline-buffer-modification-icon t
        doom-modeline-time-icon t)
  )

(use-package kaolin-themes
  :ensure t
  ;; :init (load-theme 'kaolin-galaxy t)
  )

(use-package modus-themes
  :ensure t
  :demand t)

;; https://github.com/protesilaos/ef-themes
(use-package ef-themes
  :ensure t
  :after modus-themes
  :init
  (load-theme 'ef-summer t)
  ;; (ef-themes-take-over-modus-themes-mode 1)
  :bind
  (("<f5>" . modus-themes-rotate)
   ("C-<f5>" . modus-themes-select)
   ("M-<f5>" . modus-themes-load-random))
  :config
  (setq modus-themes-mixed-fonts t)
  (setq modus-themes-italic-constructs t)

  (modus-themes-load-theme 'ef-frost)
 )

;; winum
(use-package winum
  :config (winum-mode))

(provide 'core-ui)
;;; core-ui.el ends here
