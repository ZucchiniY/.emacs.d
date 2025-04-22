;; modules-treesit.el --- Define calendar.	-*- lexical-binding: t -*-

;; Copyright (C) 2019-2025 Dylan Yang

;; Author: Dylan Yang <banshiliuli1990@sina.com>
;; URL: https://github.com/zucchiniy/.emacs.d

;;; Commentary:
;;

;;; Code:
(use-package treesit
  :ensure nil
  :preface
  (defun map-setup-install-grammars ()
    "Install Tree-sitter grammars if they are absent."
    (interactive)
    (dolist (grammar
             '(
               ;; (css . ("https://ghfast.top/https://github.com/tree-sitter/tree-sitter-css" "v0.20.0"))
               ;; (go . ("https://ghfast.top/https://github.com/tree-sitter/tree-sitter-go" "v0.20.0"))
               ;; (html . ("https://ghfast.top/https://github.com/tree-sitter/tree-sitter-html" "v0.20.1"))
               ;; (javascript . ("https://ghfast.top/https://github.com/tree-sitter/tree-sitter-javascript" "v0.20.1" "src"))
               (json . ("https://ghfast.top/https://github.com/tree-sitter/tree-sitter-json"))
               (bash . ("https://ghfast.top/https://github.com/tree-sitter/tree-sitter-bash" "v0.23.3"))
               (markdown . ("https://ghfast.top/https://github.com/ikatyang/tree-sitter-markdown"))
               (python . ("https://ghfast.top/https://github.com/tree-sitter/tree-sitter-python"))
               (rust . ("https://ghfast.top/https://github.com/tree-sitter/tree-sitter-rust" "v0.23.3"))
               (toml . ("https://ghfast.top/https://github.com/tree-sitter/tree-sitter-toml"))
               ;; (tsx . ("https://ghfast.top/https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "tsx/src"))
               ;; (typescript . ("https://ghfast.top/https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "typescript/src"))
               (yaml . ("https://ghfast.top/https://github.com/ikatyang/tree-sitter-yaml"))))
      (add-to-list 'treesit-language-source-alist grammar)
      (unless (treesit-language-available-p (car grammar))
        (treesit-install-language-grammar (car grammar)))))
  (dolist (mapping
           '((python-mode . python-ts-mode)
             ;; (css-mode . css-ts-mode)
             ;; (typescript-mode . typescript-ts-mode)
             ;; (js2-mode . js-ts-mode)
             (bash-mode . bash-ts-mode)
             (conf-toml-mode . toml-ts-mode)
             ;; (go-mode . go-ts-mode)
             ;; (css-mode . css-ts-mode)
             (rust-mode . rust-ts-mode)
             (js-json-mode . json-ts-mode)
             ))
    (add-to-list 'major-mode-remap-alist mapping))
  :config
  (map-setup-install-grammars)
  (use-package combobulate
    :custom
    (combobulate-key-prefix "C-c o")
    :hook ((prog-mode . combobulate-mode))
    :load-path "load-lisp/combobulate"))

(use-package treesit-auto
  :hook (after-init . global-treesit-auto-mode)
  :init (setq treesit-auto-install 'prompt))

(provide 'modules-treesit)
;;; modules-treesit.el ends here
