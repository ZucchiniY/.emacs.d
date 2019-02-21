;;; dylan-basic.el --- summary -*- lexical-binding: t -*-

;; Author: Dylan Yang
;; Maintainer: Dylan Yang

;;; Commentary:

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

(setq user-full-name "Dylan Yang"
      user-mail-address "banshiliuli1990@sina.com")

(when sys/winntp
  (setq w32-lwindow-modifier 'supper
	    w32-apps-modifier 'hyper)
  (w32-register-hot-key [s-t]))

(when sys/macp
  (setq mac-command-modifier 'meta
	    mac-option-modifier 'super
	    mac-control-modifier 'control
	    ns-function-modifier 'hyper))

;; UTF-8 as the default coding system
(when (fboundp 'set-charset-priority)
  (set-charset-priority 'unicode))
(prefer-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(setq locale-coding-system 'utf-8)
(setq system-time-locale "C")
(setq-default buffer-file-coding-system 'utf-8)
;; fixed Invalid coding system: cp65001
(define-coding-system-alias 'cp65001 'utf-8)

(use-package no-littering
  :init
  (setq no-littering-etc-directory (expand-file-name "config/" user-emacs-directory)
	    no-littering-var-directory (expand-file-name "data/" user-emacs-directory)))

(use-package desktop
  :ensure nil
  :init (desktop-save-mode 1)
  :config
  (setq desktop-restore-in-current-display nil))

(use-package which-key
  :diminish which-key-mode
  :hook (after-init . which-key-mode))

(use-package htmlize)

(use-package yasnippet
  :diminish yas-minor-mode
  :hook (after-init . yas-global-mode)
  :config (use-package yasnippet-snippets))

;; 隐藏滚动条
(set-scroll-bar-mode nil)
(menu-bar-mode nil)
;;(tool-bar-mode nil)
(tooltip-mode nil)
(setq make-backup-files nil)

;; text-mode
(add-hook 'text-mode-hook
	      (lambda ()
            (set-fill-column 80)
	        (turn-on-auto-fill)
	        (diminish 'auto-fill-function)))

;; abbrev-mode
(add-hook 'abbrev-mode-hook (lambda () (diminish 'abbrev-mode)))

(setq-default c-basic-offset 4
	          tab-width 4
	          indent-tabs-mode nil)

(defun dylan//set-monospaced-font (english chinese english-size chinese-size)
  "Zty//set-monospaced-font to configuration the font.
ENGLISH is english font name
CHINESE is chinese font name
ENGLISH-SIZE is the english fond size
CHINESE-SIZE is the chinese font size."
  (set-face-attribute 'default nil
                      :font (font-spec
                             :name english
                             :weight 'normal
                             :slant 'normal
                             :size english-size))
  (dolist (charset '(kana han symbol cjk-misc bopomofo))
    (set-fontset-font (frame-parameter nil 'font)
                      charset
                      (font-spec ;;:family chinese
                       :name chinese
                       :weight 'normal
                       :slant 'normal
                       :size chinese-size))))

(if (display-graphic-p)
    (if (or sys/mac-x-p sys/linux-x-p)
        (dylan//set-monospaced-font "Iosevka Term" "黑体-简" 14 14)
      (dylan//set-monospaced-font "Iosevka Term" "Microsoft YaHei" 14 14)))

(fset 'yes-or-no-p 'y-or-n-p)

;; Toggle fullscreen
(bind-keys ("C-s-f" . toggle-frame-fullscreen) ; Compatible with macOS
           ("M-S-<return>" . toggle-frame-fullscreen))

(use-package winum
  :defer 0
  :config (winum-mode))

(use-package winner
  :hook (after-init . winner-mode)
  :config
  (setq winner-boring-buffers '("*Completions*"
                                "*Compile-Log*"
                                "*Inferior-lisp*"
                                "*Fuzzy Completions*"
                                "*Apropos*"
                                "*Help*"
                                "*cvs*"
                                "*Buffer List*"
                                "*Ibuffer*"
                                "*esh command on file*")))

(use-package expand-region
  :bind ("C-=" . er/expand-region))

(use-package smart-region
  :hook (after-init . smart-region-on))

(use-package undo-tree
  :diminish undo-tree-mode
  :hook (after-init . global-undo-tree-mode))

;; Multiple cursors
(use-package multiple-cursors
  :bind (("C-S-c C-S-c"   . mc/edit-lines)
         ("C->"           . mc/mark-next-like-this)
         ("C-<"           . mc/mark-previous-like-this)
         ("C-c C-<"       . mc/mark-all-like-this)
         ("C-M->"         . mc/skip-to-next-like-this)
         ("C-M-<"         . mc/skip-to-previous-like-this)
         ("s-<mouse-1>"   . mc/add-cursor-on-click)
         ("C-S-<mouse-1>" . mc/add-cursor-on-click)
         :map mc/keymap
         ("C-|" . mc/vertical-align-with-space)))

(provide 'dylan-basic)
;;; dylan-basic.el ends here
