(setq user-full-name "Dylan Yang")

(when sys/winntp
  ;; 经过测试，在 windows 下，window 键是不能修改的
  (setq ;;w32-lwindow-modifier 'supper
	    w32-apps-modifier 'hyper)
  (w32-register-hot-key [s-t]))

(when sys/macp
  (setq mac-command-modifier 'meta
	    mac-option-modifier 'super
	    mac-control-modifier 'control
	    ns-function-modifier 'hyper))

;; UTF-8 as the default coding system
;; (when (fboundp 'set-charset-priority)
;;   (set-charset-priority 'unicode))
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

(setq-default c-basic-offset 4
	          tab-width 4
	          indent-tabs-mode nil)

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

;; text-mode
(add-hook 'text-mode-hook
	      (lambda ()
	        (turn-on-auto-fill)
	        (diminish 'auto-fill-function)))

;; abbrev-mode
(add-hook 'abbrev-mode-hook (lambda () (diminish 'abbrev-mode)))

(fset 'yes-or-no-p 'y-or-n-p)

(use-package expand-region
  :bind ("C-=" . er/expand-region))

(use-package smart-region
  :hook (after-init . smart-region-on))

(use-package undo-tree
  :diminish undo-tree-mode
  :hook (after-init . global-undo-tree-mode))

;; add keybind maximized screen to atl+return
;; add keybind fullscreen to atl+shift+return
(bind-keys ("M-S-<return>" . toggle-frame-fullscreen)
           ("M-s-<return>" . toggle-frame-maximized))


(use-package general
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
    ;; window keybinds
    "wo" 'other-window
    "wv" 'split-window-vertically
    "w-" 'split-window-horizontally
    "wl" 'evil-window-right
    "wh" 'evil-window-left
    "wk" 'evil-window-up
    "wj" 'evil-window-down
    "wq" 'delete-window))

(provide 'core-basis)
