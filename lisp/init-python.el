;;; init-python.el --- Python configuration
;;
;; Python 开发环境配置
;; 使用 ruff server (Rust 实现) 作为 LSP 服务器
;;
;; 安装步骤：
;; 1. 安装 ruff: pip install ruff
;;

;;; Code:
(use-package python
  :functions exec-path-from-shell-copy-env
  :hook (inferior-python-mode . (lambda ()
                                  (process-query-on-exit-flag
                                   (get-process "Python"))))
  :init
  (setq python-indent-offset 4
        python-indent-guess-indent-offset nil)
  :config
  ;; 使用 ruff server 作为 Python LSP 后端
  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs
                 '((python-ts-mode python-mode)
                   . ("ruff" "server"))))
  ;; 使用 ruff 作为 linter 和 formatter
  (when (executable-find "ruff")
    (use-package flymake-ruff
      :hook (python-base-mode . flymake-ruff-mode)))
  (when (and (executable-find "python3")
             (string= python-shell-interpreter "python"))
    (setq python-shell-interpreter "python3"))
  (with-eval-after-load 'exec-path-from-shell
    (exec-path-from-shell-copy-env "PYTHONPATH"))
  )

(provide 'init-python)
;;; init-python.el ends here
