;;; init-rust.el --- Rust configuration
;;
;; Rust 开发环境配置
;; 使用 rust-analyzer 作为 LSP 服务器
;;
;; 安装步骤：
;; 1. 安装 rust: curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
;; 2. 安装 rust-analyzer: rustup component add rust-analyzer
;;

;;; Code:
(use-package rust-mode
  :mode ("\\.rs\\'" . rust-mode)
  :init (setq rust-format-on-save t
              rust-mode-treesitter-derive t)
  :config
  (add-hook 'rust-mode-hook (lambda () (setq indent-tabs-mode nil)))
  (define-derived-mode rustic-mode rust-mode "Rust"
    "Major mode for Rust code."))

(use-package toml-mode
  :defer t
  :mode ("\\.toml\\'" . toml-mode))

(use-package cargo
  :defer t)

(use-package ron-mode
  :defer t
  :mode ("\\.ron\\'" . ron-mode))

(provide 'init-rust)
;;; init-rust.el ends here
