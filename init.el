(defun update-load-path (&rest _)
  "Update `load-path'."
  (push (expand-file-name "site-lisp" user-emacs-directory) load-path)
  (push (expand-file-name "lisp" user-emacs-directory) load-path))
(advice-add #'package-initialize :after #'update-load-path)

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(update-load-path)

;; Set package archives
(setq package-archives '(("gnu"   . "http://elpa.emacs-china.org/gnu/")
			 ("melpa" . "http://elpa.emacs-china.org/melpa/")
			 ("org" . "http://elpa.emacs-china.org/org/")))

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

(use-package diminish)
(use-package bind-key)

(use-package package-utils
	     :init
	     (defalias 'upgrade-packages 'package-utils-upgrade-all)
	     (defalias 'upgrade-packages-and-restart 'package-utils-upgrade-all-and-restart))

(declare-function upgrade-packages-and-restart 'init-package)
(defalias 'dylan-update-packages-and-restartup 'upgrade-packages-and-restart)

(require 'dylan-basic)
(require 'dylan-org)
(require 'dylan-magit)
(require 'dylan-expansion)
(require 'dylan-company)

(provide 'init)
