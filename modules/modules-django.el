;; modules-django.el --- Define django.	-*- lexical-binding: t -*-

;; Copyright (C) 2019-2021 Dylan Yang

;; Author: Dylan Yang <banshiliuli1990@sina.com>
;; URL: https://github.com/zucchiniy/.emacs.d

;;; Commentary:
;;
;;; Code:

(use-package pony-mode
  :defer t
  :general
  (general-define-key
   :states 'normal
   :keymaps 'python-mode-map
   :prefix "SPC m"
   "jaf" 'pony-fabric
   "jad" 'pony-fabric-deploy

   "jfs" 'pony-goto-settings
   "jfc" 'pony-setting
   "jft" 'pony-goto-template
   "jfr" 'pony-resolve

   "jid" 'pony-db-shell
   "jis" 'pony-shell

   "jm" 'pony-manage

   "jrd" 'pony-stopserver
   "jro" 'pony-browser
   "jrr" 'pony-restart-server
   "jru" 'pony-runserver
   "jrt" 'pony-temp-server

   "jsc" 'pony-south-convert
   "jsh" 'pony-south-schemamigration
   "jsi" 'pony-south-initial
   "jsm" 'pony-south-migrate
   "jss" 'pony-syncdb

   "jtd" 'pony-test-down
   "jte" 'pony-test-goto-err
   "jto" 'pony-test-open
   "jtt" 'pony-test
   "jtu" 'pony-test-up))
(provide 'modules-django)
;;; modules-django.el ends here
