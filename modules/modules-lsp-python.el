;;; package --- Summary
;;; Commentary:
;;; config lsp mode
;;; Code:
(use-package lsp-mode
  :init
  ;; (setq lsp-keymap-prefix "SPC l")
  :hook ((python-mode . lsp-deferred)
         ;; (javascript-mode . lsp-deferred)
         ;; (typescript-mode .lsp-deferred)
         (lsp-mode . lsp-enable-which-key-integration))
  :commands (lsp lsp-deferred))

;; debugger
(use-package dap-mode
  :after lsp-mode
  :commands (dap-debug dap-register-debug-template)
  :hook ((python-mode . dap-ui-mode)
         (python-mode . dap-mode))
  :custom
  (dap-auto-configure-mode)
  (dap-auto-configure-feature
   '(sessions locals breakpoints expressions tooltip))
  :config
  (require 'dap-python)
  ;; (require 'dap-javascript)
  ;; (require 'dap-typescript)
  (dap-register-debug-template
   "Python dap"
   (list :type "python"
         :args "-i"
         :cwd nil
         :env '(("DEBUG" . "1"))
         ;; :target-module (expand-file-name "")
         :request "launch"
         ;; :name "

         )
   )
  )

;; treemacs and ivy
(use-package lsp-treemacs :config (lsp-treemacs-sync-mode 1))
(use-package lsp-ivy)

;; lsp-python
(use-package lsp-pyright
  :ensure t
  :hook (python-mode . (lambda ()
                         (require 'lsp-pyright)
                         (lsp-deferred))))

(provide 'modules-lsp-python)
;;; modules-lsp-python.el ends here
