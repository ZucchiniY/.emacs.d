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
 tencent-elpa
  '(("gnu"   . "http://mirrors.cloud.tencent.com/elpa/gnu/")
    ("melpa" . "http://mirrors.cloud.tencent.com/elpa/melpa/")
    ("org" . "http://mirrors.cloud.tencent.com/elpa/org/"))
  ustc-elpa
  '(("gnu"   . "http://mirrors.ustc.edu.cn/elpa/gnu/")
    ("melpa" . "http://mirrors.ustc.edu.cn/elpa/melpa/")
    ("org" . "http://mirrors.ustc.edu.cn/elpa/org/")))

(provide 'core-variable)
;;; core-variable.el ends here
