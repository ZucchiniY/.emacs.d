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
  (require 'init-funcs)
  )

;; Nerd Icons
(use-package nerd-icons
  :demand t
  :custom
  (nerd-icons-font-family "IosevkaTerm NFM"))

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
			                (height . 0.85)))

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
  (add-hook 'after-load-theme-hook #'refresh-ns-appearance))

;; 在 modeline/tabline/headerline 上显示 gc 信息，gc 次数 - gc 时间
(setq-default
 header-line-format
 '("GC: " (:eval (number-to-string gcs-done)) " - " (:eval (number-to-string gc-elapsed)) "s"))

(cond (sys/mac-x-p (dylan//set-monospaced-font "IosevkaTerm NFM" "LXGW WenKai Mono GB Screen" 16 16))
      (sys/linux-x-p (dylan//set-monospaced-font "IosevkaTerm NFM" "LXGW WenKai Mono GB Screen" 18 18))
      (sys/win-x-p (dylan//set-monospaced-font "Iosevka Nerd Font Mono" "Microsoft YaHei" 14 14)))

;; 启用默认主题
(load-theme 'modus-operandi t)

;; 使用默认的 mode-line
(setq mode-line-format
      '((:eval (if (buffer-modified-p) "*" ""))
        " %b "
        (:eval (if buffer-read-only "%R" ""))
        " %-12.12f "
        "%m"))

;; 简化模式行显示
(setq mode-line-modes
      '((:propertize mode-name
                     display (minimal-mode-line-mode-name))))

;; Show line numbers
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(add-hook 'conf-mode-hook 'display-line-numbers-mode)
(setq display-line-numbers-width-start t)

;; Suppress GUI features
(setq use-file-dialog nil
      use-dialog-box nil
      inhibit-startup-screen t
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

;; 文本缩放键绑定
(global-set-key (kbd "s-=") #'text-scale-increase)
(global-set-key (kbd "s--") #'text-scale-decrease)
(global-set-key (kbd "s-0") #'text-scale-reset)
(global-set-key (kbd "C-s-=") #'text-scale-increase)
(global-set-key (kbd "C-s--") #'text-scale-decrease)
(global-set-key (kbd "C-s-0") #'text-scale-reset)

;; Display time
(display-time-mode 1)
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
      ;; mouse
      mouse-wheel-scroll-amount-horizontal 1
      mouse-wheel-progressive-speed nil)

;; For macOS
(when sys/macp
  ;; Render thinner fonts
  (setq ns-use-thin-smoothing t)
  ;; Don't open a file in a new frame
  (setq ns-pop-up-frames nil))

(provide 'init-ui)
;;; init-ui.el ends here
