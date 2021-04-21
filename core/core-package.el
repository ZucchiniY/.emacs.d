;; Set package archives
(setq package-archives '(("gnu"   . "http://mirrors.cloud.tencent.com/elpa/gnu/")
                         ("melpa" . "http://mirrors.cloud.tencent.com/elpa/melpa/")
                         ("org" . "http://mirrors.cloud.tencent.com/elpa/org/")))

;; Initialize packages
(unless (bound-and-true-p package--initialized)
  (setq package-enable-at-startup nil)
  (package-initialize))

;; use package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
;; Should set before loading `use-package'

(setq use-package-always-ensure t)
(setq use-package-always-defer t)
(setq use-package-expand-minimally t)
(setq use-package-enable-imenu-support t)

(eval-when-compile
  (require 'use-package))

;; add all-the-icons package
(use-package all-the-icons)
(use-package diminish)
(use-package bind-key)

;; use package-utils to update packages
(use-package package-utils
	     :init
	     (defalias 'upgrade-packages 'package-utils-upgrade-all)
	     (defalias 'upgrade-packages-and-restart 'package-utils-upgrade-all-and-restart))

(declare-function upgrade-packages-and-restart 'init-package)
(defalias 'dylan-update-packages-and-restartup 'upgrade-packages-and-restart)

(provide 'core-package)
