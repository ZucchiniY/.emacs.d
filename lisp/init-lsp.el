;;; init-lsp.el --- LSP configuration
;;
;; 使用 Eglot 作为 LSP 客户端
;; 语言服务器后端：
;; - Rust: rust-analyzer
;; - Python: ruff server
;; - JavaScript/TypeScript: deno lsp
;; - Dart: dart analysis-server
;;
;; 配置说明：
;; - eglot-autoshutdown t: 空闲时自动关闭 LSP 服务器
;; - eglot-events-buffer-size 0: 禁用事件缓冲区以节省内存
;; - eglot-send-changes-idle-time 0.5: 0.5秒后发送变更
;;

;;; Code:
(use-package eglot
  :hook ((prog-mode . (lambda ()
                        (unless (derived-mode-p
                                 'emacs-lisp-mode 'lisp-mode
                                 'makefile-mode 'snippet-mode
                                 'ron-mode)
                          (eglot-ensure))))
         ((markdown-mode yaml-mode yaml-ts-mode) . eglot-ensure)
         ((c-ts-mode c-mode) . eglot-ensure)
         ((c++-mode c++-ts-mode) . eglot-ensure)
         ((c-or-c++-mode c-or-c++-ts-mode) . eglot-ensure)
         ((python-mode python-ts-mode) . eglot-ensure)
         ((rust-mode rust-ts-mode) . eglot-ensure)
         ((js-mode js-ts-mode typescript-mode typescript-ts-mode) . eglot-ensure)
         ((dart-mode) . eglot-ensure))
  :init
  (setq eglot-autoshutdown t
        eglot-events-buffer-size 0
        eglot-send-changes-idle-time 0.5)
  :config
  (when (fboundp 'eglot-ensure)
    (eglot-ensure))
  (add-to-list 'eglot-stay-out-of 'flymake)
  :general
  (general-define-key
   :states 'normal
   :keymaps 'override
   :prefix "SPC e"
   "d" 'xref-find-definitions
   "r" 'eglot-rename
   "h" 'eldoc
   "R" 'eglot-reconnect
   "f" 'eglot-format
   "ca" 'eglot-code-actions
   "cq" 'eglot-code-action-quickfix
   "ci" 'eglot-code-action-inline
   "cr" 'eglot-code-action-rewrite
   "co" 'eglot-code-action-organize-imports
   )
  )

;; 加速 eglot
(use-package eglot-booster
  :ensure nil
  :after eglot
  :config (eglot-booster-mode))

(provide 'init-lsp)
;;; init-lsp.el ends here
