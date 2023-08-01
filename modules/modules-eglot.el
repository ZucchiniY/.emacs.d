;;; package --- Summary
;;; Commentary:
;;; config lsp mode
;;; Code:
(use-package eglot
  :commands eglot-ensuer
  :hook
  ((python-mode
    javascript-mode
    typescript-ts-mode
    tsx-ts-mode
    web-mode) . eglot-ensure)
  :config
  (add-to-list 'eglot-server-programs '(python-mode . ("pyright")))
  (add-to-list 'eglot-server-programs '((js-ts-mode tsx-ts-mode typescript-ts-mode)  . ("")))
  )

(use-package consult-eglot
  :after eglot
  :commands (conusel-eglot-symbols))

(use-package eldoc-box
  :after eglot
  :hook (eglot-managed-mode . eldoc-box-hover-at-point-mode))

(defalias 'dylan/eglot
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "a") #'eglot-code-actions)
    (define-key map (kbd "f") #'eglot-format)
    (define-key map (kbd "r") #'eglot-rename)
    map)
  "eglot keymap")
(global-set-key (kbd "C-c e") 'dylan/eglot)

(use-package lsp-pyright
  :hook (python-mode . (lambda ()
                         (require 'lsp-pyright)
                         (lsp))))

(provide 'modules-eglot)
;;; modules-eglot.el ends here
