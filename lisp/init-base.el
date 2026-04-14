;; init-base.el --- Define basis config.	-*- lexical-binding: t -*-

;; Copyright (C) 2019-2021 Dylan Yang

;; Author: Dylan Yang <banshiliuli1990@sina.com>
;; URL: https://github.com/zucchiniy/.emacs.d

;;; Commentary:
;;
;; basis variables and basis configuration.

;;; Code:
(eval-when-compile
  (require 'init-const))

(setq user-full-name "Dylan Yang"
      user-mail-address "banshiliuli1990@sina.com")

;; Key Modifiers
(cond
 (sys/winntp
  (setq w32-lwindow-modifier 'super ; left windows key
        w32-apps-modifier 'hyper)   ; menu/app key
  (w32-register-hot-key [s-t]))
 (sys/mac-port-p
  (setq mac-option-modifier 'meta
        mac-command-modifier 'super)
  (global-set-key [(super a)] 'mark-whole-buffer)
  (global-set-key [(super c)] 'kill-ring-save)
  (global-set-key [(super l)] 'goto-line)
  (global-set-key [(super q)] 'save-buffers-kill-emacs)
  (global-set-key [(super s)] 'save-buffer)
  (global-set-key [(super v)] 'yank)
  (global-set-key [(super w)] 'delete-frame)
  (global-set-key [(super z)] 'undo)))

;; 增加单个块中从进程中读取的数据量, 默认为4kb
(setq read-process-output-max #x100000)
;; 默认占用的栈大小为 1600 ，调整为 16000
(setq max-lisp-eval-depth 16000)
;; 不要 ping 域名
(setq ffap-machine-p-known 'reject)

;; 显示 error 以上错误
(setq warning-minimum-level :error)

;; Garbage Collector settings
(setq gc-cons-threshold #x1000000
      gc-cons-percentage 0.1)

;; SET UTF-8 as the default coding system
(when (fboundp 'set-charset-priority)
  (set-charset-priority 'unicode))
(set-language-environment "UTF-8")
(prefer-coding-system 'utf-8)
(setq coding-system-for-read 'utf-8
      coding-system-for-write 'utf-8)

;; 设置键盘输入时的编码
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(set-next-selection-coding-system 'utf-8)
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

;; Enable saveplace
(save-place-mode 1)

;; Enable recentf
(recentf-mode 1)
(setq recentf-max-saved-items 300
      recentf-exclude
      '("\\.?cache" ".cask" "url" "COMMIT_EDITMSG\\'" "bookmarks"
        "\\.\\(?:gz\\|gif\\|svg\\|png\\|jpe?g\\|bmp\\|xpm\\)$"
        "\\.?ido\\.last$" "\\.revive$" "/G?TAGS$" "/.elfeed/"
        "^/tmp/" "^/var/folders/.+$" "^/ssh:" "/persp-confs/"
        (lambda (file) (file-in-directory-p file package-user-dir)))
      recentf-save-file (expand-file-name "recentf" user-emacs-directory))
(push (expand-file-name recentf-save-file) recentf-exclude)
(add-to-list 'recentf-filename-handlers #'abbreviate-file-name)
(global-set-key (kbd "C-x C-r") 'recentf-open-files)

;; Enable savehist
(savehist-mode 1)
(setq enable-recursive-minibuffers t
      history-length 1000
      savehist-additional-variables '(mark-ring
                                      global-mark-ring
                                      search-ring
                                      regexp-search-ring
                                      extended-command-history)
      savehist-autosave-interval 300)

;; Enable size indication mode
(size-indication-mode 1)

;; Basic settings
(setq column-number-mode t
      line-number-mode t
      kill-whole-line t               ; Kill line including '\n'
      line-move-visual nil
      track-eol t                     ; Keep cursor at end of lines. Require line-move-visual is nil.
      set-mark-command-repeat-pop t)  ; Repeating C-SPC after popping mark pops it again

;; Visualize TAB, (HARD) SPACE, NEWLINE
(setq-default show-trailing-whitespace nil) ; Don't show trailing whitespace by default
(defun enable-trailing-whitespace ()
  "Show trailing spaces and delete on saving."
  (setq show-trailing-whitespace t)
  (add-hook 'before-save-hook #'delete-trailing-whitespace nil t))

;; Enable visual line mode for text mode
(add-hook 'text-mode-hook 'visual-line-mode)
;; Enable trailing whitespace for prog modes
(add-hook 'prog-mode-hook 'enable-trailing-whitespace)

;; misc
(if (boundp 'use-short-answers)
    (setq use-short-answers t)
  (fset 'yes-or-no-p 'y-or-n-p))
(setq-default major-mode 'text-mode
              fill-column 80
              tab-width 4
              indent-tabs-mode nil)

(setq visible-bell t
      inhibit-compacting-font-caches t  ; Don’t compact font caches during GC
      delete-by-moving-to-trash t       ; Deleting files go to OS's trash folder
      make-backup-files nil             ; Forbide to make backup files
      auto-save-default nil             ; Disable auto save

      uniquify-buffer-name-style 'post-forward-angle-brackets ; Show path if names are same
      adaptive-fill-regexp "[ t]+|[ t]*([0-9]+.|*+)[ t]*"
      adaptive-fill-first-line-regexp "^* *$"
      sentence-end "\\([。！？]\\|……\\|[.?!][]\"')}]*\\($\\|[ \t]\\)\\)[ \t\n]*"
      sentence-end-double-space nil
      word-wrap-by-category t)

;; Asynchronous processing
;; 保留默认的异步处理设置

;; Frame
(when (display-graphic-p)
  ;; Frame fullscreen
  (global-set-key (kbd "M-s-<return>") #'toggle-frame-maximized)
  (global-set-key (kbd "S-s-<return>") #'toggle-frame-fullscreen)
  (and sys/mac-x-p (global-set-key (kbd "C-s-f") #'toggle-frame-fullscreen)))

;; Global keybindings
(global-set-key (kbd "s-r") #'revert-buffer-quick)
(global-set-key (kbd "C-x K") #'delete-this-file)
(global-set-key (kbd "C-c C-l") #'reload-init-file)

(provide 'init-base)
;;; init-base.el ends here
