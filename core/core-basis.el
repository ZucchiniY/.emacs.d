;; core-basis.el --- Define basis config.	-*- lexical-binding: t -*-

;; Copyright (C) 2019-2021 Dylan Yang

;; Author: Dylan Yang <banshiliuli1990@sina.com>
;; URL: https://github.com/zucchiniy/.emacs.d

;;; Commentary:
;;
;; basis variables and basis configuration.

;;; Code:
(defconst sys/winntp
  (eq system-type 'windows-nt))

(defconst sys/linuxp
  (eq system-type 'gnu/linux))

(defconst sys/macp
  (eq system-type 'darwin))

(defconst sys/linux-x-p
  (and (display-graphic-p) sys/linuxp))
(defconst sys/mac-x-p
  (and (display-graphic-p) sys/macp))

(defconst tuna-elpa
  '(("gnu"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
    ("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
    ("no-gnu" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/nongnu/")))

(defconst ustc-elpa
  '(("gnu"   . "http://mirrors.ustc.edu.cn/elpa/gnu/")
    ("melpa" . "http://mirrors.ustc.edu.cn/elpa/melpa/")
    ("melpa-stable" . "http://mirrors.ustc.edu.cn/elpa/melpa-stable/")))

(setq user-full-name "Dylan Yang")
(set-default 'truncate-lines nil)

;; config function keybind
(when sys/winntp
  ;; 经过测试，在 windows 下，window 键是不能修改的
  ;;w32-lwindow-modifier 'supper
  (setq w32-apps-modifier 'hyper
        w32-register-hot-key [s-t]))

(when sys/macp
  (setq mac-command-modifier 'meta
	    mac-option-modifier 'super
	    mac-control-modifier 'control
	    ns-function-modifier 'hyper))

;; SET UTF-8 as the default coding system
(set-language-environment 'Chinese-GB)
(prefer-coding-system 'utf-8)
(setq coding-system-for-read 'utf-8
      coding-system-for-write 'utf-8)

;; 设置键盘输入时的编码
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
;; 设置文件默认保存的编码
(set-buffer-file-coding-system 'utf-8)
(setq-default buffer-file-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
;; 解决粘贴中文出现乱码问题
(set-clipboard-coding-system 'utf-8)
;; 其它乱码问题
(set-terminal-coding-system 'utf-8)
(set-file-name-coding-system 'utf-8)
(setq locale-coding-system 'utf-8)
;; 保持星期使用英文
(setq system-time-locale "C")
;; fixed Invalid coding system: cp65001
(when sys/winntp
  (define-coding-system-alias 'cp65001 'utf-8))

;; set default tab and space
(setq-default c-basic-offset 4
              python-indent-offset 4
	          tab-width 4
	          indent-tabs-mode nil)

(fset 'yes-or-no-p 'y-or-n-p)

;; add keybind maximized screen to atl+return
;; add keybind fullscreen to atl+shift+return
;; (global-set-key (kbd "M-S-<return>") 'toggle-frame-fullscreen)
(global-set-key (kbd "M-s-<return>") 'toggle-frame-maximized)

(provide 'core-basis)
;;; core-basis.el ends here

