;; init-ui.el --- Define ui config.	-*- lexical-binding: t -*-

;; Copyright (C) 2019-2021 Dylan Yang

;; Author: Dylan Yang <banshiliuli1990@sina.com>
;; URL: https://github.com/zucchiniy/.emacs.d

;;; Commentary:
;;

;;; Code:
;; 隐藏滚动条、菜单栏
(eval-when-compile
  (require 'init-const)
  (require 'init-custom))

;; 隐藏菜单和工具栏
(setq-default tool-bar-always-show-default nil)
(when (display-graphic-p)
  (tool-bar-mode -1)
  (menu-bar-mode -1)
  (scroll-bar-mode -1))

(setq idle-update-delay 1.0)

(setq-default cursor-in-non-selected-windows nil)
(setq highlight-nonselected-windows nil)

(setq fast-but-imprecise-scrolling t)
(setq redisplay-skip-fontification-on-input t)

(setq frame-inhibit-implied-resize t
      frame-resize-pixelwise t)

(setq initial-frame-alist '((top . 0.5)
                            (left . 0.5)
                            (width . 0.7)
                            (height . 0.85)
                            (fullscreen)))

;; 不禁止启动屏幕，这样 dashboard 才能显示
;; (setq fancy-splath-image dylan-logo)

;; 设置成透明窗口
(set-frame-parameter nil 'alpha '(95 . 100))

(setq frame-title-format '("Dylan's Emacs - %b")
      icon-title-format frame-title-format)

(when sys/mac-port-p
  (add-hook 'server-after-make-frame-hook
            (lambda ()
              (if (display-graphic-p)
                  (menu-bar-mode 1)
                (menu-bar-mode -1))))
  (defun refresh-ns-appearance ()
    "Safely refresh frame parameter `ns-appearance' to match background mode."
    (interactive)
    (let ((bg (frame-parameter nil 'background-mode)))
      (set-frame-parameter nil 'ns-appearance bg)
      (setf (alist-get 'ns-appearance default-frame-alist) bg)))

  ;; Hook up appearance refresh to theme changes
  (add-hook 'after-load-theme-hook #'refresh-ns-appearance)

  (with-eval-after-load 'auto-dark
    (dolist (hook '(auto-dark-dark-mode-hook auto-dark-light-mode-hook))
      (add-hook hook #'refresh-ns-appearance))))

;; 在 modeline/tabline/headerline 上显示 gc 信息，gc 次数 - gc 时间
(setq-default
 header-line-format
 '("GC: " (:eval (number-to-string gcs-done)) " - " (:eval (number-to-string gc-elapsed)) "s"))

(cond (sys/mac-x-p (dylan//set-monospaced-font "IosevkaTerm NFM" "LXGW WenKai Mono GB Screen" 16 16))
      (sys/linux-x-p (dylan//set-monospaced-font "IosevkaTerm NFM" "LXGW WenKai Mono GB Screen" 18 18))
      (sys/win-x-p (dylan//set-monospaced-font "Iosevka Nerd Font Mono" "Microsoft YaHei" 14 14)))

;; https://github.com/protesilaos/ef-themes
(use-package modus-themes
  :init
  (modus-themes-include-derivatives-mode 1)
  :config
  (setq modus-themes-to-toggle '(modus-operandi modus-vivendi)
        modus-themes-to-rotate modus-themes-items
        modus-themes-variable-pitch-ui t
        modus-themes-italic-constructs t
        modus-themes-bold-constructs t
        modus-themes-completions '((t . (bold)))
        modus-themes-prompts '(bold)
        modus-themes-headings
        '((agenda-structure . (variable-pitch light 2.2))
          (agenda-date . (variable-pitch regular 1.3))
          (t . (regular 1.15))))

  (setq modus-themes-common-palette-overrides nil)
  )

(use-package ef-themes
  :after modus-themes
  :init
  (ef-themes-take-over-modus-themes-mode 1)
  :config
  (setq modus-themes-italic-constructs t)
  )

(use-package circadian
  :after ef-themes
  :commands circadian-setup
  :custom (circadian-themes dylan-auto-themes)
  :init
  (setq calendar-latitude 39.54)
  (setq calendar-longitude 116.25)
  (circadian-setup)
  )

(use-package doom-modeline
  :after (nerd-icons)
  :hook (after-init . doom-modeline-mode)
  :init
  (setq doom-modeline-icon dylan-icon
        doom-modeline-minor-modes t
        doom-modeline-major-mode-color-icon t
        doom-modeline-major-mode-icon t
        doom-modeline-buffer-state-icon t
        doom-modeline-buffer-modification-icon t
        doom-modeline-time-icon t))

(use-package minions
  :hook after-init)

;; Icons
(use-package nerd-icons
  :demand t
  :custom
  (nerd-icons-font-family "IosevkaTerm NFM"))

;; Show line numbers
(use-package display-line-numbers
  :ensure nil
  :hook ((prog-mode
          conf-mode toml-ts-mode
          yaml-mode yaml-ts-mode)
         . display-line-numbers-mode)
  :init (setq display-line-numbers-width-start t))

;; Suppress GUI features - 不禁止启动屏幕
(setq use-file-dialog nil
      use-dialog-box nil
      inhibit-startup-echo-area-message user-login-name
      inhibit-default-init t
      initial-scratch-message nil)
(unless (daemonp)
  (advice-add #'display-startup-echo-area-message :override #'ignore))

;; Display dividers between windows
(setq window-divider-default-places t
      window-divider-default-bottom-width 1
      window-divider-default-right-width 1)
(add-hook 'window-setup-hook #'window-divider-mode)

;; Easily adjust the font size in all frames
(use-package default-text-scale
  :hook (after-init . default-text-scale-mode)
  :bind (:map default-text-scale-mode-map
         ("s-="   . default-text-scale-increase)
         ("s--"   . default-text-scale-decrease)
         ("s-0"   . default-text-scale-reset)
         ("C-s-=" . default-text-scale-increase)
         ("C-s--" . default-text-scale-decrease)
         ("C-s-0" . default-text-scale-reset)))

;; Display time
(setq display-time-default-load-average nil
      display-time-format "%H:%M")

;; Scrolling
;; Scroll one line at a time (less "jumpy" than defaults)
(setq hscroll-step 1
      hscroll-margin 2
      scroll-step 1
      scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position t
      auto-window-vscroll nil
      mouse-wheel-scroll-amount-horizontal 1
      mouse-wheel-progressive-speed nil)

(use-package page-break-lines
  :diminish
  :hook (after-init . global-page-break-lines-mode))

;; For macOS
(with-no-warnings
  (when sys/macp
    ;; Render thinner fonts
    (setq ns-use-thin-smoothing t)
    ;; Don't open a file in a new frame
    (setq ns-pop-up-frames nil)))

(provide 'init-ui)
;;; init-ui.el ends here
