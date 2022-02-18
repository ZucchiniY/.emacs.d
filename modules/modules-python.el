;;; modules-python.el --- summary -*- lexical-binding: t -*-

;; Author: Dylan Yang
;; Maintainer: Dylan Yang
;; Version: 1.0.0
;; Package-Requires: ( )
;; Homepage: 
;; Keywords: 



;;; Commentary:

;; 

;;; Code:
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

(use-package anaconda-mode
  :commands anaconda-mode
  :diminish anaconda-mode
  :hook ((python-mode . anaconda-mode)
         (python-mode . anaconda-eldoc-mode)
         ))

(defun python/python-mode-hook ()
  "python module hook."
  (anaconda-mode 1)
  (anaconda-eldoc-mode 1))

(add-hook 'python-mode-hook 'python/python-mode-hook)

(provide 'modules-python)

;;; modules-python.el ends here
