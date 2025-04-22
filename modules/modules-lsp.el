;;; package --- Summary
;;; Commentary:
;;; config lsp mode
;;; Code:
;; lsp-python
(use-package lsp-bridge
  :commands (global-lsp-bridge-mode)
  :load-path "load-lisp/lsp-bridge"
  :init
  (setq lsp-bridger-python-comand (expand-file-name (concat user-emacs-directory "load-lisp/lsp-bridge/.venv/bin/python")))
  (global-lsp-bridge-mode))

(provide 'modules-lsp)
;;; modules-lsp.el ends here
