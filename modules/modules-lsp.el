;;; package --- Summary
;;; Commentary:
;;; config lsp mode
;;; Code:
;; lsp-python
(use-package lsp-bridge
  :load-path "load-lisp/lsp-bridge"
  :defer 5
  :after (yasnippet markdown-mode)
  :config
  (setq lsp-bridge-python-lsp-server "ruff")
  :init
  (global-lsp-bridge-mode)
  (setq lsp-bridger-python-comand (expand-file-name (concat user-emacs-directory "load-lisp/lsp-bridge/.venv/bin/python"))))

(provide 'modules-lsp)
;;; modules-lsp.el ends here
