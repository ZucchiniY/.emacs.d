;; core-variable.el --- Define basis variable.	-*- lexical-binding: t -*-

;; Copyright (C) 2019-2021 Dylan Yang

;; Author: Dylan Yang <banshiliuli1990@sina.com>
;; URL: https://github.com/zucchiniy/.emacs.d

;;; Commentary:
;;

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

(setf
 tuna-elpa
  '(("gnu"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
    ("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
    ("no-gnu" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/nongnu/"))
  ustc-elpa
  '(("gnu"   . "http://mirrors.ustc.edu.cn/elpa/gnu/")
    ("melpa" . "http://mirrors.ustc.edu.cn/elpa/melpa/")
    ("melpa-stable" . "http://mirrors.ustc.edu.cn/elpa/melpa-stable/")))

(provide 'core-variable)
;;; core-variable.el ends here
