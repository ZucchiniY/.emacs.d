;; Emacs development
(use-package elisp-mode
  :ensure nil
  :bind (:map emacs-lisp-mode-map
              ("C-c C-x" . ielm)
              ("C-c C-c" . eval-defun)
              ("C-c C-b" . eval-buffer)))

(use-package eldoc
  :ensure nil
  :diminish eldoc-mode)

(provide 'modules-elisp)
