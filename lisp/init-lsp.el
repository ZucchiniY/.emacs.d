;;; package --- Summary
;;; Commentary:
;;; config lsp mode
;;; Code:
;; lsp-python
(use-package lspce
  :load-path "load-lisp/lspce"
  :hook ((rust-mode . lspce-mode)
         ;; (c-mode . lspce-mode)
         (python-mode . lspce-mode))
  :config
  (progn
    (setq lspce-send-changed-idle-time 0.1
          lspce-idle-delay 0.1)
    (setq lspce-show-log-level-in-modeline t
          lspce-eldoc-enable-hover nil
          lspce-eldoc-enable-signature t)
    (setq eldoc-idle-delay 0.1)
    ;; (add-hook 'lspce-mode-hook #'lspce-inlay-hints-mode)
    (lspce-set-log-file "~/.emacs.d/lspce.log")
    (lspce-enable-logging)
    (setq lspce-sever-programs
          `(("rust" "rust-analyzer" "" lspce-ra-initializationOptions)
            ("C" "clangd" "--all-scopes-completion --clang-tidy --enable-config --header-insertion-decorators=0")
            ("typescript" "deno" "" lspce-deno-initializationOptions)
            ("javascript" "deno" "" lspce-deno-initializationOptions)
            ("python" "pylsp" "" lspce-pylsp-initializationOptions)))
    :general
    (general-define-key
     :states '(normal visual emacs)
     :keymaps 'override
     :prefix "SPC l"
     "d" 'xref-find-definitions
     "r" 'xref-find-references
     "a" 'lspce-code-actions)
    ))

(provide 'init-lsp)
;;; init-lsp.el ends here
