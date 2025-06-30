;;; package --- Summary
;;; Commentary:
;;; config lsp mode
;;; Code:
;; lsp-python
(use-package lspce
  :load-path "load-lisp/lspce"
  :hook ((rust-mode . lspce-mode)
         (python-mode . lspce-mode))
  :config
  (setq lspce-send-changed-idle-time 0.1
        lspce-idle-delay 0.1)
  (setq lspce-show-log-level-in-modeline t
        lspce-eldoc-enable-hover nil
        lspce-eldoc-enable-signature t)
  (setq eldoc-idle-delay 0.1)
  (add-hook 'lspce-mode-hook #'lspce-inlay-hints-mode)
        
  (lspce-set-log-file "~/.emacs.d/lspce.log")
  (setq lspce-sever-programs `(
                               ("rust" "rust-analyzer" "" lspce-ra-initializationOptions)
                               ;; ("rustic" "rust-analyzer" "" lspce-ra-initializationOptionns)
                               ("python" "pylsp" "")))
  :general
  (general-define-key
   :states '(normal visual emacs)
   :keymaps 'override
   :prefix "SPC l"
   "d" 'xref-find-definitions
   "r" 'xref-find-references
   "a" 'lspce-code-actions)
   )

(provide 'modules-lsp)
;;; modules-lsp.el ends here
