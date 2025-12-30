;;; init-python.el --- summary -*- lexical-binding: t -*-

;; Copyright (C) 2019-2026 Dylan Yang

;; Author: Dylan Yang <banshiliuli1990@sina.com>
;; URL: https://github.com/zucchiniy/.emacs.d

;;; Commentary:
;;
;; python configuration
;;; Code:
(use-package python
  :functions exec-path-from-shell-copy-env
  :hook (inferior-python-mode . (lambda ()
                                  (process-query-on-exit-flag
                                   (get-process "Python"))))

  :init
  (setq python-chell-completion-native-enable nil)
  :config
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
