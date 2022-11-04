;; core-package.el --- Define Package config.	-*- lexical-binding: t -*-

;; Copyright (C) 2019-2021 Dylan Yang

;; Author: Dylan Yang <banshiliuli1990@sina.com>
;; URL: https://github.com/zucchiniy/.emacs.d

;;; Commentary:
;;

;;; Code:
;; Set package archives and use-package.
(require 'core-basis)

(setq package-archives tuna-elpa)

;; Initialize packages
(unless (bound-and-true-p package--initialized)
  (setq package-enable-at-startup nil)
  (package-initialize))

;; use package
(eval-when-compile
  (add-to-list 'load-path "site-lisp/use-package")
  (require 'use-package))

;; abbrev-mode
(add-hook 'abbrev-mode-hook (lambda () (diminish 'abbrev-mode)))

;; add all-the-icons package
(use-package all-the-icons
  :load-path "site-lisp/all-the-icons"
  :defer t)

(use-package diminish :defer t)
(use-package bind-key :defer t)

;; use package-utils to update packages
(use-package package-utils
	     :init
	     (defalias 'upgrade-packages 'package-utils-upgrade-all)
	     (defalias 'upgrade-packages-and-restart 'package-utils-upgrade-all-and-restart))

(use-package no-littering
  :init
  (setq no-littering-etc-directory (expand-file-name "config/" user-emacs-directory)
	    no-littering-var-directory (expand-file-name "data/" user-emacs-directory)))

(use-package which-key
  :diminish which-key-mode
  :hook (after-init . which-key-mode))

(use-package htmlize)

(use-package yasnippet
  :diminish yas-minor-mode
  :hook (after-init . yas-global-mode)
  :config (use-package yasnippet-snippets))

(use-package expand-region
  :bind ("C-=" . er/expand-region))

(use-package smart-region
  :hook (after-init . smart-region-on))

(declare-function upgrade-packages-and-restart 'init-package)
(defalias 'dylan-update-packages-and-restartup 'upgrade-packages-and-restart)

(use-package recentf
  :defer t
  :config
  (recentf-mode t))

(use-package general
  :defer 1
  :commands general-override-states
  :init
  (setq general-override-states '(insert
                                  emacs
                                  hybrid
                                  normal
                                  visual
                                  motion
                                  operator
                                  replacea))
  :config
  (general-evil-setup t)
  (general-create-definer global-leader
    :states '(normal visual motion)
    :prefix "SPC"
    :keymaps 'override)
  (general-create-definer local-leader
    :states '(normal visual motion)
    :prefix "SPC m"
    :keymaps 'override)
  (global-leader
    ;; files keybinds
    "fs" 'save-buffer
    "fd" 'dired
    ;; buffer keybinds
    "bk" 'kill-buffer
    ;; quite emacs
    "qq" 'save-buffers-kill-emacs
    ;; winner
    "wu" 'winner-undo
    "wr" 'winner-redo
    ;; window keybinds
    "wo" 'other-window
    "wv" 'split-window-vertically
    "w-" 'split-window-horizontally
    "wl" 'evil-window-right
    "wh" 'evil-window-left
    "wk" 'evil-window-up
    "wj" 'evil-window-down
    "wq" 'delete-window
    "wa" 'delete-other-windows
    ;; windows select
     "1" 'winum-select-window-1
     "2" 'winum-select-window-2
     "3" 'winum-select-window-3
     "4" 'winum-select-window-4
     "5" 'winum-select-window-5
     "6" 'winum-select-window-6
     "7" 'winum-select-window-7
     "8" 'winum-select-window-8
     "9" 'winum-select-window-9
     "0" 'winum-select-window-0-or-10
     ;; writeroom
     "rw" 'writeroom-mode
     ;; cfw::open-org-calendar
     "ov" 'cfw:open-org-calendar
    ))


(provide 'core-package)
;;; core-package.el ends here
