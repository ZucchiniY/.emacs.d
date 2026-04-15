;; init-completion.el --- Define completion config.	-*- lexical-binding: t -*-

;; Copyright (C) 2019-2021 Dylan Yang

;; Author: Dylan Yang <banshiliuli1990@sina.com>
;; URL: https://github.com/zucchiniy/.emacs.d

;;; Commentary:
;;
;; Modern completion configuration.
;; Use Corfu/Consult for completion.
;; https://github.com/AboutEmacs/consult/blob/main/README_zh.org

;;; Code:
(eval-when-compile
  (require 'init-const))

;; 无顺序的补全样式
(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles basic partial-completion))
                                  (org-roam-node (styles orderless))))
  (orderless-component-separator #'orderless-escapable-split-on-space))

;; 垂直交互式补全
(use-package vertico
  :custom (vertico-count 15)
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  :hook ((after-init . vertico-mode)
         (rfn-eshadow-update-overlay . vertico-directory-tidy)))

;; 为现有命令添加补全注释
(use-package marginalia
  :hook (after-init . marginalia-mode))

;; 提供各种咨询功能
(use-package consult
  :defines (xref-show-xrefs-function xref-show-definitions-function)
  :defines shr-color-html-colors-alist
  :autoload (consult-register-format consult-register-window consult-xref)
  :autoload (consult--read consult--customize-put)
  :commands (consult-narrow-help)
  :functions (list-colors-duplicates consult-colors--web-list)
  :general
  (general-define-key
   :states 'normal
   :prefix "SPC c" ;; 补全相关功能前缀
   :doc "补全相关功能"
   "M-x" 'consult-mode-command     ;; 命令执行
   "h"   'consult-history          ;; 历史记录
   "i"   'consult-info             ;; 信息查询
   "r"   'consult-ripgrep          ;; 正则搜索
   "t"   'consult-theme            ;; 主题选择
   "m"   'consult-imenu            ;; 跳转到函数/变量
   "M"   'consult-imenu-multi      ;; 多文件跳转到函数/变量
   "M-:" 'consult-complex-command  ;; 复杂命令
   "d"   'consult-dir              ;; 目录选择
   "s"   'consult-flyspell         ;; 拼写错误
   "f"   'consult-find             ;; 文件查找
   "l"   'consult-line             ;; 行查找
   "g"   'consult-grep             ;; 内容搜索
   "G"   'consult-git-grep         ;; Git内容搜索
   "k"   'consult-keep-lines       ;; 保留匹配行
   "F"   'consult-focus-lines      ;; 聚焦匹配行
   "I"   'consult-isearch-history  ;; 搜索历史
   )
  (general-define-key
   :states 'normal
   :prefix "SPC b" ;; 缓冲区相关前缀
   :doc "缓冲区相关功能"
   "b"   'consult-buffer            ;; 缓冲区选择
   "o"   'consult-buffer-other-window ;; 新窗口打开缓冲区
   )
  (general-define-key
   :states 'normal
   :prefix "SPC p" ;; 项目相关前缀
   :doc "项目相关功能"
   "b" 'consult-project-buffer     ;; 项目缓冲区
   )
  (general-define-key
   :states 'normal
   :prefix "SPC r" ;; 寄存器相关前缀
   :doc "寄存器相关功能"
   "r" 'consult-register           ;; 寄存器选择
   "l" 'consult-register-load      ;; 加载寄存器
   "s" 'consult-register-store     ;; 存储到寄存器
   )
  (general-define-key
   :states 'normal
   :prefix "SPC j" ;; 跳转相关前缀
   :doc "跳转相关功能"
   "e" 'consult-compile-error      ;; 编译错误
   "f" 'consult-flymake           ;; Flymake错误
   "l" 'consult-goto-line         ;; 跳转到行
   "o" 'consult-outline           ;; 大纲跳转
   )
  :bind (("C-c c e" . consult-colors-emacs)
         ("C-c c w" . consult-colors-webb)
         ("C-c c f" . describe-face)
         ("C-c c l" . find-library)
         ("M-y" . consult-yank-pop)
         ([remap Info-search]        . consult-info)
         ([remap isearch-forward]    . consult-line)
         ([remap recentf-open-files] . consult-recent-file)
         )

  ;; Enable automatic preview at point in the *Completions* buffer.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init
  ;; Optionally configure the register formatting.
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (with-eval-after-load 'xref
    (setq xref-show-xrefs-function #'consult-xref
          xref-show-definitions-function #'consult-xref))

  ;; More utils
  (defvar consult-colors-history nil
    "History for `consult-colors-emacs' and `consult-colors-web'.")

  ;; No longer preloaded in Emacs 28.
  (autoload 'list-colors-duplicates "facemenu")
  ;; No preloaded in consult.el
  (autoload 'consult--read "consult")

  (defun consult-colors-emacs (color)
    "Show a list of all supported colors for a particular frame.

You can insert the name (default), or insert or kill the hexadecimal or RGB
value of the selected COLOR."
    (interactive
     (list (consult--read (list-colors-duplicates (defined-colors))
                          :prompt "Emacs color: "
                          :require-match t
                          :category 'color
                          :history '(:input consult-colors-history))))
    (insert color))

  ;; Adapted from counsel.el to get web colors.
  (defun consult-colors--web-list nil
    "Return list of CSS colors for `counsult-colors-web'."
    (require 'shr-color)
    (sort (mapcar #'downcase (mapcar #'car shr-color-html-colors-alist)) #'string-lessp))

  (defun consult-colors-web (color)
    "Show a list of all CSS colors.\

You can insert the name (default), or insert or kill the hexadecimal or RGB
value of the selected COLOR."
    (interactive
     (list (consult--read (consult-colors--web-list)
                          :prompt "Color: "
                          :require-match t
                          :category 'color
                          :history '(:input consult-colors-history))))
    (insert color))
  :config
  ;; Optionally configure preview.
  (setq consult-preview-key nil)
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-line consult-line-multi :preview-key 'any
   consult-buffer consult-recent-file consult-theme :preview-key '(:debounce 1.0 any)
   consult-goto-line :preview-key '(:debounce 0.5 any)
   consult-ripgrep consult-git-grep consult-grep
   :initial (selected-region-or-symbol-at-point)
   :preview-key '(:debounce 0.5 any))

  ;; Optionally configure the narrowing key.
  (setq consult-narrow-key "<") ;; "C-+"

  ;; Optionally make narrowing help available in the minibuffer.
  (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help))

;; 增强 minibuffer 操作功能
(use-package embark
  :commands embark-prefix-help-command
  :general
  (general-define-key
   :states 'normal
   :prefix "SPC e" ;; Embark 相关功能前缀
   :doc "Embark 相关功能"
   "a" 'embark-act          ;; 对当前目标执行操作
   "d" 'embark-dwim         ;; 智能执行操作
   "b" 'embark-bindings     ;; 显示绑定
   )
  :bind (
         ("s-.'"   . embark-act)
         ("C-s-.'" . embark-act)
         ([remap describe-bindings] . embark-bindings)
         :map minibuffer-local-map
         ("M-.'" . my-embark-preview)
         )
  :init
  ;; 可选：使用 completing-read 界面替换按键帮助
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  ;; 为非 Consult 命令提供手动预览功能
  (defun my-embark-preview ()
    "在 vertico 缓冲区中预览候选项，除非是 consult 命令。"
    (interactive)
    (unless (bound-and-true-p consult--preview-function)
      (save-selected-window
        (let ((embark-quit-after-action nil))
          (embark-dwim)))))

  ;; 隐藏 Embark 实时/补全缓冲区的模式行
  (add-to-list 'display-buffer-alist
               '(("\\`\\*Embark Collect \\(Live\\|Completions\\\\)\\*""
                 nil
                 (window-parameters (mode-line-format . none)))))

  (with-no-warnings
    (with-eval-after-load 'which-key
      (defun embark-which-key-indicator ()
        "使用 which-key 显示 keymaps 的 embark 指示器。
which-key 帮助信息将显示当前目标的类型和值，
如果有更多目标，则显示省略号。"
        (lambda (&optional keymap targets prefix)
          (if (null keymap)
              (which-key--hide-popup-ignore-command)
            (which-key--show-keymap
             (if (eq (plist-get (car targets) :type) 'embark-become)
                 "变为"
               (format "对 %s '%s'%s 执行操作"
                       (plist-get (car targets) :type)
                       (embark--truncate-target (plist-get (car targets) :target))
                       (if (cdr targets) "…" "")))
             (if prefix
                 (pcase (lookup-key keymap prefix 'accept-default)
                   ((and (pred keymapp) km) km)
                   (_ (key-binding prefix 'accept-default)))
               keymap)
             nil nil t (lambda (binding)
                         (not (string-suffix-p "-argument" (cdr binding))))))))

      (setq embark-indicators
            '(embark-which-key-indicator
              embark-highlight-indicator
              embark-isearch-highlight-indicator))

      (defun embark-hide-which-key-indicator (fn &rest args)
        "在使用 completing-read 提示器时立即隐藏 which-key 指示器。"
        (which-key--hide-popup-ignore-command)
        (let ((embark-indicators
               (remq #'embark-which-key-indicator embark-indicators)))
          (apply fn args)))

      (advice-add #'embark-completing-read-prompter
                  :around #'embark-hide-which-key-indicator))))

;; Embark 与 Consult 集成
(use-package embark-consult
  :bind (:map minibuffer-mode-map
              ("C-c C-o" . embark-export))
  :hook (embark-collect-mode . consult-preview-at-point-mode))

;; 自动补全
(use-package corfu
  :autoload (corfu-quit consult-completion-in-region)
  :functions (persistent-scratch-save corfu-move-to-minibuffer)
  :custom
  (corfu-auto nil) ;; 禁用自动补全，仅在按下 M-Tab 时触发
  (corfu-count 12)
  (corfu-preview-current nil)
  (corfu-on-exact-match nil)
  (global-corfu-modes '((not erc-mode
                             circe-mode
                             help-mode
                             gud-mode
                             vterm-mode)
                        t))
  :bind (
         ("M-/" . completion-at-point) ;; 绑定 M-/ 触发补全
         ("TAB" . completion-at-point) ;; 绑定 Tab 触发补全
         :map corfu-map
         ("M-m" . corfu-move-to-minibuffer) ;; 移动补全到 minibuffer
         )
  :hook ((after-init . global-corfu-mode)
         (global-corfu-mode . corfu-history-mode))
  :config
  (add-hook 'before-save-hook #'corfu-quit)
  (advice-add #'persistent-scratch-save :before #'corfu-quit)
  (defun corfu-move-to-minibuffer ()
    (interactive)
    (pcase completion-in-region--data
      (`(,beg ,end ,table ,pred ,extras)
       (let ((completion-extra-properties extras)
             completion-cycle-threshold completion-cycling)
         (consult-completion-in-region beg end table pred)))))
  (add-to-list 'corfu-continue-commands #'corfu-move-to-minibuffer))

(unless (or (display-graphic-p) (featurep 'tty-child-frames))
  (use-package corfu-terminal
    :hook (global-corfu-mode . corfu-terminal-mode)))

;; 为补全添加图标支持
(use-package nerd-icons-completion
  :after corfu
  :hook (corfu-mode . nerd-icons-completion-mode)
  :config
  (setq nerd-icons-completion-iconify-all t))

;; 目录补全增强
(use-package consult-dir
  :ensure t
  :general
  (general-define-key
   :states 'normal
   :prefix "SPC c"
   :doc "补全相关功能"
   "d" 'consult-dir              ;; 目录选择
   )
  )

;; 拼写错误补全
(use-package consult-flyspell
  :general
  (general-define-key
   :states 'normal
   :prefix "SPC c"
   :doc "补全相关功能"
   "s" 'consult-flyspell         ;; 拼写错误
   )
  )

;; 代码片段补全
(use-package consult-yasnippet
  :general
  (general-define-key
   :states 'normal
   :prefix "SPC c"
   :doc "补全相关功能"
   "y" 'consult-yasnippet        ;; 代码片段补全
   )
  )

;; 括号补全配置
;; 仅在特定文件类型的 hook 中启用
(use-package emacs
  :custom
  (electric-pair-mode nil) ;; 全局禁用
  :hook
  ;; 在编程模式中启用括号补全
  (prog-mode . electric-pair-mode)
  ;; 在 Org 模式中启用括号补全
  (org-mode . electric-pair-mode)
  :config
  ;; 配置括号补全的行为
  (setq electric-pair-pairs '(;; 基本括号
                              (?' . ?')
                              (?) . ?")
                              (?\` . ?\`)
                              (?[ . ?])
                              (?{ . ?}))
        electric-pair-inhibit-predicate
        (lambda (c) (if (char-equal c ?")
                       (not (in-string-p))
                     t)))

;; 基本配置
(use-package emacs
  :custom
  ;; 启用 TAB 补全
  (tab-always-indent 'complete)
  ;; Emacs 30+：禁用 Ispell 补全
  (text-mode-ispell-word-completion nil)
  ;; Emacs 28+：在 M-x 中隐藏不适用的命令
  (read-extended-command-predicate #'command-completion-default-include-p))

;; 补全增强
(use-package cape
  :commands (cape-file cape-elisp-block cape-keyword)
  :autoload (cape-wrap-noninterruptible cape-wrap-nonexclusive cape-wrap-buster)
  :autoload (cape-wrap-silent)
  :init
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-elisp-block)
  (add-to-list 'completion-at-point-functions #'cape-keyword)

  ;; Make these capfs composable.
  (advice-add 'lsp-completion-at-point :around #'cape-wrap-noninterruptible)
  (advice-add 'lsp-completion-at-point :around #'cape-wrap-nonexclusive)
  (advice-add 'comint-completion-at-point :around #'cape-wrap-nonexclusive)
  (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster)
  (advice-add 'eglot-completion-at-point :around #'cape-wrap-nonexclusive)
  (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-nonexclusive)

  ;; Sanitize the `pcomplete-completions-at-point' Capf.
  (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-silent))

(provide 'init-completion)
;;; init-completion.el ends here