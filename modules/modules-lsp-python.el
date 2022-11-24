;;; config lsp mode
(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "SPC l")
  :hook ((python-mode . lsp-deferred)
         ;; (javascript-mode . lsp-deferred)
         ;; (typescript-mode .lsp-deferred)
         (lsp-mode . lsp-enable-which-key-integration))
  :commands (lsp lsp-deferred))

;; debugger
(use-package dap-mode)
(use-package dap-python)
;; (use-package dap-javascript)
;; (use-package dap-typescript)

;; treemacs and ivy
(use-package lsp-treemacs)
(use-package lsp-ivy)

;; lsp-python
(use-package lsp-pyright
  :ensure t
  :hook (python-mode . (lambda ()
                         (require 'lsp-pyright)
                         (lsp-deferred))))

(provide 'modules-lsp-python)
