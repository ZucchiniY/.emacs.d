;; init-utils.el --- Initialize ultilities.	-*- lexical-binding: t -*-

;; Author: Dylan Yang
;; URL: https://github.com/zucchiniy/.emacs.d

;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; Some usefule Utilities.
;;

;;; Code:

(eval-when-compile
  (require 'init-const))

;; 显示可用键绑定弹出窗口
(use-package which-key
  :diminish
  :functions childframe-completion-workable-p
  :bind ("C-h M-m" . which-key-show-major-mode)
  :hook (after-init . which-key-mode)
  :init (setq which-key-max-description-length 30
              which-key-lighter nil
              which-key-show-remaining-keys t)
  )

;; 搜索工具
(use-package grep
  :ensure nil
  :autoload grep-apply-setting
  :init
  (when (executable-find "rg")
    (grep-apply-setting
     'grep-command "rg --color=auto --null -nH --no-heading -e ")
    (grep-apply-setting
     'grep-template "rg --color=auto --null --no-heading -g '!*/' -e <R> <D>")
    (grep-apply-setting
     'grep-find-command '("rg --color=auto --null -nH --no-heading -e ''" . 38))
    (grep-apply-setting
     'grep-find-template "rg --color=auto --null -nH --no-heading -e <R> <D>")))

;; 可编辑的 grep 区域
(use-package wgrep
  :init (setq wgrep-auto-save-buffer t
              wgrep-change-readonly-file t))

;; Fast search tool `ripgrep'
(use-package rg
  :hook (after-init . rg-enable-default-bindings)
  :bind (:map rg-global-map
              ("c" . rg-dwim-current-dir)
              ("f" . rg-dwim-current-file)
              ("m" . rg-menu))
  :init (setq rg-show-columns t)
  :config (add-to-list 'rg-custom-type-aliases '("tmpl" . "*.tmpl")))

;; 优雅的写作模式
(use-package olivetti
  :diminish
  :general
  (global-leader
    "lo" 'olivetti-mode
    )
  :init (setq olivetti-body-width 0.62))

;; 文本模式树
(use-package ztree
  :custom-face
  (ztreep-header-face ((t (:inherit diff-header :foreground unspecified))))
  (ztreep-arrow-face ((t (:inherit font-lock-comment-face :foreground unspecified))))
  (ztreep-leaf-face ((t (:inherit diff-index :foreground unspecified))))
  (ztreep-node-face ((t (:inherit font-lock-variable-name-face :foreground unspecified))))
  (ztreep-expand-sign-face ((t (:inherit font-lock-function-name-face))))
  (ztreep-diff-header-face ((t (:inherit (diff-header bold :foreground unspecified)))))
  (ztreep-diff-header-small-face ((t (:inherit diff-file-header :foreground unspecified))))
  (ztreep-diff-model-normal-face ((t (:inherit font-lock-doc-face :foreground unspecified))))
  (ztreep-diff-model-ignored-face ((t (:inherit font-lock-doc-face :strike-through t :foreground unspecified))))
  (ztreep-diff-model-diff-face ((t (:inherit diff-removed :foreground unspecified))))
  (ztreep-diff-model-add-face ((t (:inherit diff-nonexistent :foreground unspecified))))
  :general
  (general-define-key
   :states '(normal visual)
   :keymaps 'override
   :prefix "SPC z"
   ;; diff
   "C" 'ztree-diff-copy
   "h" 'ztree-diff-toggle-show-equal-files
   "H" 'ztree-diff-toggle-show-filtered-files
   "D" 'ztree-diff-delete-file
   "v" 'ztree-diff-view-file
   "d" 'ztree-diff-simple-diff-files
   "r" 'ztree-diff-partial-rescan
   "R" 'ztree-diff-full-rescan
   ;; view
   "RET" 'ztree-perform-action
   "SPC" 'ztree-perform-soft-action
   "TAB" 'ztree-jump-side
   "g" 'ztree-refresh-buffer
   "x" 'ztree-toggle-expand-subtree
   "<backspace>" 'ztree-move-up-in-tree
   )
  :init (setq ztree-draw-unicode-lines t
              ztree-show-number-of-children t))

;; 环境变量列表
(use-package list-environment
  :init
  (with-no-warnings
    (defun my-list-environment-entries ()
      "Generate environment variable entries list for tabulated-list."
      (mapcar (lambda (env)
                (let* ((kv (split-string env "="))
                       (key (car kv))
                       (val (mapconcat #'identity (cdr kv) "=")))
                  (list key (vector
                             `(,key face font-lock-keyword-face)
                             `(,val face font-lock-string-face)))))
              process-environment))
    (advice-add #'list-environment-entries :override #'my-list-environment-entries)))

(provide 'init-utils)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-utils.el ends here
