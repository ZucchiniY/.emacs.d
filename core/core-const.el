;; core-const.el --- Define basis variable.	-*- lexical-binding: t -*-

;; Copyright (C) 2019-2021 Dylan Yang

;; Author: Dylan Yang <banshiliuli1990@sina.com>
;; URL: https://github.com/zucchiniy/.emacs.d

;;; Commentary:
;;
;; Define constants.
;; 定义常量

;;; Code:
(defconst dylan-homepage
  "https://github.com/ZucchiniY"
  "The Github page of Dylan Emacs.")

(defconst sys/winntp
  (eq system-type 'windows-nt))

(defconst sys/linuxp
  (eq system-type 'gnu/linux))

(defconst sys/macp
  (eq system-type 'darwin))

(defconst sys/win-x-p
  (and (display-graphic-p) sys/winntp))
(defconst sys/linux-x-p
  (and (display-graphic-p) sys/linuxp))
(defconst sys/mac-x-p
  (and (display-graphic-p) sys/macp))

(defconst tuna-elpa
  '(("gnu"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
    ("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
    ("nongnu" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/nongnu/")
    ))

(defconst ustc-elpa
  '(("gnu" . "https://mirrors.ustc.edu.cn/elpa/gnu/")
    ("melpa" . "https://mirrors.ustc.edu.cn/elpa/melpa/")
    ("nongnu" . "https://mirrors.ustc.edu.cn/elpa/nongnu/")))

(provide 'core-const)
;;; core-const.el ends here
