;;; init-rust.el --- summary -*- lexical-binding: t -*-

;; Copyright (C) 2019-2026 Dylan Yang

;; Author: Dylan Yang <banshiliuli1990@sina.com>
;; URL: https://github.com/zucchiniy/.emacs.d

;;; Commentary:
;;
;; rust configuration
;;; Code:

(use-package rust-mode
  :mode ("\\.rs\\'" . rust-mode)
  :init (setq rust-format-on-save t
              rust-mode-treesitter-derive t)
  :config
  (add-hook 'rust-mode-hook (lambda () (setq indent-tabs-mode nil)))
  (define-derived-mode rustic-mode rust-mode "Rust"
    "Major mode for Rust code."))

(use-package toml-mode :defer t)
(use-package cargo :defer t)
(use-package ron-mode
  :defer t
  :mode ("\\.ron" . ron-mode))

(provide 'init-rust)

;;; init-rust.el ends here
