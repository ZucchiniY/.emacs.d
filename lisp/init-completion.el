;; init-completion.el --- Define company config.	-*- lexical-binding: t -*-

;; Copyright (C) 2019-2021 Dylan Yang

;; Author: Dylan Yang <banshiliuli1990@sina.com>
;; URL: https://github.com/zucchiniy/.emacs.d

;;; Commentary:
;;
;; Modern completion configuration.
;; delete company and use Corfu/Consult to completion.
;; https://github.com/AboutEmacs/consult/blob/main/README_zh.org

;;; Code:
(eval-when-compile
  (require 'init-const))

;; Optionally use the `orderless' completion style.
(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles basic partial-completion))))
  (orderless-component-separator #'orderless-escapable-split-on-space))

;; VERTical Interactive COmpletion
(use-package vertico
  :custom (vertico-count 15)
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  :hook ((after-init . vertico-mode)
         (rfn-eshadow-update-overlay . vertico-directory-tidy)))

;; Display vertico in the child frame
(use-package vertico-posframe
  :functions posframe-poshandler-frame-center-near-bottom
  :hook (vertico-mode . vertico-posframe-mode)
  :init (setq vertico-posframe-poshandler
              #'posframe-poshandler-frame-center-near-bottom
              vertico-posframe-parameters
              '((left-fringe  . 8)
                (right-fringe . 8))))

;; Enrich existing commands with completion annotations
(use-package marginalia
  :hook (after-init . marginalia-mode))

;; Add icons to completion candidates
(use-package nerd-icons-completion
  :hook (marginalia-mode . nerd-icons-completion-marginalia-setup))

;; Consulting completing-read
(use-package consult
  :defines (xref-show-xrefs-function xref-show-definitions-function)
  :defines shr-color-html-colors-alist
  :autoload (consult-register-format consult-register-window consult-xref)
  :autoload (consult--read consult--customize-put)
  :commands (consult-narrow-help)
  :functions (list-colors-duplicates consult-colors--web-list)
  :bind (;; C-c bindings in `mode-specific-map'
         ("C-c M-x" . consult-mode-command) ;; 运行当前 mode 命令，可以通过 l/g/m 对应的本地、全局、主要模式
         ("C-c h"   . consult-history)
         ("C-c k"   . consult-kmacro) ;; 宏列表中选择宏并执行
         ("C-c i"   . consult-info)   ;; 从 info 页面中进行全文搜索
         ("C-c r"   . consult-ripgrep) ;; 使用 ripgrep 进行搜索
         ("C-c T"   . consult-theme)  ;; 选择主题并禁用所有当前启用的主题
         ("C-."     . consult-imenu)  ;; 跳转到当前项目缓冲区中的 Imenu

         ("C-c c e" . consult-colors-emacs) ;; 查看 Emacs 可用颜色
         ("C-c c w" . consult-colors-web) ;; 查看 Web 可用颜色
         ("C-c c f" . describe-face) ;; 查看当前样式属性
         ("C-c c l" . find-library) ;; 中转到对应的库文件
         ("C-c c t" . consult-theme) ;; 选择主题

         ([remap Info-search]        . consult-info)
         ([remap isearch-forward]    . consult-line)
         ([remap recentf-open-files] . consult-recent-file)

         ;; C-x bindings in `ctl-x-map'
         ("C-x M-:" . consult-complex-command)     ;; 从 `command-history' 中选择命令
         ("C-x b"   . consult-buffer)              ;; 快速切换 buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; 快速切换 window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; 快速切换 frame
         ("C-x r b" . consult-bookmark)            ;; 选择或者创建书签
         ("C-x p b" . consult-project-buffer)      ;; 选择切换到 project
         ;; Custom M-# bindings for fast register access
         ("M-#"     . consult-register-load)       ;; 快速加载寄存器
         ("M-'"     . consult-register-store)      ;; 根据当前上下文快速存储到寄存器
         ("C-M-#"   . consult-register)            ;; 从寄存器列表中选择，可以快速缩小类型和位置
         ;; Other custom bindings
         ("M-y"     . consult-yank-pop)            ;; 快速使用 yank-pop
         ;; M-g bindings in `goto-map'
         ("M-g e"   . consult-compile-error)       ;; 跳转到编译缓冲区的编译错误
         ("M-g f"   . consult-flymake)             ;; 跳转到 Flymake 的诊断
         ("M-g g"   . consult-goto-line)           ;; 跳转到行号，支持实时预览
         ("M-g o"   . consult-outline)             ;; 跳转到大纲标题，Alternative: consult-org-heading
         ("M-g m"   . consult-mark)                ;; 跳转到 mark-ring 中的标记，支持预览和递归
         ("M-g k"   . consult-global-mark)         ;; 跳转到 global-mark-ring 中的标记，支持预览和递归
         ("M-g i"   . consult-imenu)               ;; 跳转到当前缓冲区的 imenu，支持预览、递归、缩小
         ("M-g I"   . consult-imenu-multi)         ;; 跳转到项目缓冲区的 imenu，支持预览、递归和缩小
         ;; M-s bindings in `search-map'
         ("M-s d"   . consult-find)                ;; 通过路径与正则表达式查找文件
         ("M-s D"   . consult-locate)              ;; 在 locate 中进行搜索
         ("M-s g"   . consult-grep)                ;; 在文件中正则搜索文件，使用 grep
         ("M-s G"   . consult-git-grep)            ;; 在 git 仓库中搜索文件进行搜索
         ("M-s r"   . consult-ripgrep)             ;; 在文件中正则搜索文件，使用 ripgrep
         ("M-s l"   . consult-line)                ;; 在缓冲区中搜索行
         ("M-s L"   . consult-line-multi)          ;; 多个缓冲区中进行搜索
         ("M-s k"   . consult-keep-lines)          ;; 当前缓冲区中搜索行
         ("M-s u"   . consult-focus-lines)         ;; 在缓冲区中搜索然后聚焦到搜索到的内容
         ;; Isearch integration
         ("M-s e"   . consult-isearch-history)     ;; 从 Isearch 中进行搜索
         :map isearch-mode-map
         ("M-e"   . consult-isearch-history)       ;; 从 Isearch 中进行搜索
         ("M-s e"   . consult-isearch-history)     ;; 从 Isearch 中进行搜索
         ("M-s l"   . consult-line)                ;; 从缓冲区中进行搜索
         ("M-s L"   . consult-line-multi)          ;; 从多个缓冲区中进行搜索

         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                  ;; orig. next-matching-history-element
         ("M-r" . consult-history))                 ;; orig. previous-matching-history-element

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init
  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
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
  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key '("S-<down>" "S-<up>"))
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
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; "C-+"

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help))

(use-package consult-dir
  :ensure t
  :bind (("C-x C-d" . consult-dir)  ;; 选择目录进行跳转
         :map minibuffer-local-completion-map
         ("C-x C-d" . consult-dir)  ;; 选择目录进行跳转
         ("C-x C-j" . consult-dir-jump-file))) ;; 打开当前目录并选择文件跳转

(use-package consult-flyspell
  :bind ("M-g s" . consult-flyspell)) ;; 显示当前拼写错误

(use-package consult-yasnippet
  :bind ("M-g y" . consult-yasnippet)) ;; 展开 yasnippet 模板

(use-package embark
  :commands embark-prefix-help-command
  :bind (("s-."   . embark-act)
         ("C-s-." . embark-act)
         ("M-."   . embark-dwim)        ; overrides `xref-find-definitions'
         ([remap describe-bindings] . embark-bindings)
         :map minibuffer-local-map
         ("M-." . my-embark-preview))
  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  ;; Manual preview for non-Consult commands using Embark
  (defun my-embark-preview ()
    "Previews candidate in vertico buffer, unless it's a consult command."
    (interactive)
    (unless (bound-and-true-p consult--preview-function)
      (save-selected-window
        (let ((embark-quit-after-action nil))
          (embark-dwim)))))

  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none))))

  (with-no-warnings
    (with-eval-after-load 'which-key
      (defun embark-which-key-indicator ()
        "An embark indicator that displays keymaps using which-key.
The which-key help message will show the type and value of the
current target followed by an ellipsis if there are further
targets."
        (lambda (&optional keymap targets prefix)
          (if (null keymap)
              (which-key--hide-popup-ignore-command)
            (which-key--show-keymap
             (if (eq (plist-get (car targets) :type) 'embark-become)
                 "Become"
               (format "Act on %s '%s'%s"
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
        "Hide the which-key indicator immediately when using the completing-read prompter."
        (which-key--hide-popup-ignore-command)
        (let ((embark-indicators
               (remq #'embark-which-key-indicator embark-indicators)))
          (apply fn args)))

      (advice-add #'embark-completing-read-prompter
                  :around #'embark-hide-which-key-indicator))))

(use-package embark-consult
  :bind (:map minibuffer-mode-map
              ("C-c C-o" . embark-export))
  :hook (embark-collect-mode . consult-preview-at-point-mode))

;; Auto completion
(use-package corfu
  :autoload (corfu-quit consult-completion-in-region)
  :functions (persistent-scratch-save corfu-move-to-minibuffer)
  :custom
  (corfu-auto t)
  (corfu-auto-prefix 2)
  (corfu-count 12)
  (corfu-preview-current nil)
  (corfu-on-exact-match nil)
  (corfu-auto-delay 0.2)
  (corfu-popupinfo-delay '(0.4 . 0.2))
  (global-corfu-modes '((not erc-mode
                             circe-mode
                             help-mode
                             gud-mode
                             vterm-mode)
                        t))
  :custom-face
  (corfu-border ((t (:inherit region :background unspecified))))
  :bind ("M-/" . completion-at-point)
  :hook ((after-init . global-corfu-mode)
         (global-corfu-mode . corfu-popupinfo-mode)
         (global-corfu-mode . corfu-history-mode))
  :config
  ;;Quit completion before saving
  (add-hook 'before-save-hook #'corfu-quit)
  (advice-add #'persistent-scratch-save :before #'corfu-quit)

  ;; Move completions to minibuffer
  (defun corfu-move-to-minibuffer ()
    (interactive)
    (pcase completion-in-region--data
      (`(,beg ,end ,table ,pred ,extras)
       (let ((completion-extra-properties extras)
             completion-cycle-threshold completion-cycling)
         (consult-completion-in-region beg end table pred)))))
  (keymap-set corfu-map "M-m" #'corfu-move-to-minibuffer)
  (add-to-list 'corfu-continue-commands #'corfu-move-to-minibuffer))

(unless (or (display-graphic-p) (featurep 'tty-child-frames))
  (use-package corfu-terminal
    :hook (global-corfu-mode . corfu-terminal-mode)))

;; A few more useful configurations...
(use-package emacs
  :custom
  ;; TAB cycle if there are only few candidates
  ;; (completion-cycle-threshold 3)

  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (tab-always-indent 'complete)

  ;; Emacs 30 and newer: Disable Ispell completion function. As an alternative,
  ;; try `cape-dict'.
  (text-mode-ispell-word-completion nil)

  ;; Emacs 28 and newer: Hide commands in M-x which do not apply to the current
  ;; mode.  Corfu commands are hidden, since they are not used via M-x. This
  ;; setting is useful beyond Corfu.
  (read-extended-command-predicate #'command-completion-default-include-p))

(use-package nerd-icons-corfu
  :autoload nerd-icons-corfu-formatter
  :after corfu
  :init (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

;; Add extensions
(use-package cape
  :commands (cape-file cape-elisp-block cape-keyword)
  :autoload (cape-wrap-noninterruptible cape-wrap-nonexclusive cape-wrap-buster)
  :autoload (cape-wrap-silent)
  :init
  ;; (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-elisp-block)
  (add-to-list 'completion-at-point-functions #'cape-keyword)
  ;; (add-to-list 'completion-at-point-functions #'cape-abbrev)

  ;; Make these capfs composable.
  (advice-add 'lsp-completion-at-point :around #'cape-wrap-noninterruptible)
  (advice-add 'lsp-completion-at-point :around #'cape-wrap-nonexclusive)
  (advice-add 'comint-completion-at-point :around #'cape-wrap-nonexclusive)
  (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster)
  (advice-add 'eglot-completion-at-point :around #'cape-wrap-nonexclusive)
  (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-nonexclusive)

  ;; Sanitize the `pcomplete-completions-at-point' Capf.
  ;; The Capf has undesired side effects on Emacs 28.
  (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-silent))

(provide 'init-completion)
;;; init-completion.el ends here
