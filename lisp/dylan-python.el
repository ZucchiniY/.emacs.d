;;; dylan-python.el --- summary -*- lexical-binding: t -*-

;; Author: Dylan Yang
;; Maintainer: Dylan Yang
;;; Commentary:

;;; Code:

(use-package python-mode
  :mode ("\\.py\\'" . python-mode)
  :config
  (use-package elpy
    :init
    (elpy-enable)
    (when (require 'flycheck nil t)
      (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
      (add-hook 'elpy-mode-hook 'flycheck-mode))
    (setq python-shell-interpreter "python"
        python-shell-interpreter-args "-i"))
  (use-package py-autopep8
    :hook (elpy-mode . py-autopep8-enable-on-save)))

(use-package quickrun
  :bind ("C-c x" . quickrun))

(provide 'dylan-python)

;;; dylan-python.el ends here
