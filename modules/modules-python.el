(use-package exec-path-from-shell :defer t)

(use-package python
  :mode ("\\.py\\'" . python-mode)
  :init
  (setq python-shell-completion-native-enable nil)
  (setq-default python-indent-offset 4
                python-indent-guess-indent-offset-verbose nil)
  :commands (python-mode run-python)
  ;; :intercept ("python3" . python-mode)
  :config
  (with-eval-after-load 'exec-path-from-shell
    (exec-path-from-shell-copy-env "PYTHONPATH")))

(use-package py-isort
  :init
  (setq python-sort-imports-on-save t)
  (defun +python/python-sort-imports ()
    (interactive)
    (when (and python-sort-imports-on-save
               (derived-mode-p 'python-mode))
      (py-isort-before-save)))
  (add-hook 'python-mode-hook
            (lambda () (add-hook 'before-save-hook #'+python/python-sort-imports))))

(use-package lsp-python-ms
  :hook (python-mode . (lambda ()
                        (require 'lsp-python-ms) (lsp-deferred)))
  :after lsp-mode python
  :custom
  (lsp-python-ms-dir (expand-file-name "extends/mspyls/" user-emacs-directory))
  ;; :init
  ;; (setq lsp-python-ms-executable (expand-file-name "extends/mspyls/Microsoft.Python.LanguageServer" user-emacs-directory))
  ;; (executable-find lsp-python-ms-executable)
  )


(provide 'modules-python)
