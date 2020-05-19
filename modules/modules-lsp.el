(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :custom
  (lsp-idle-delay 0.5)
  (lsp-flycheck-live-reporting nil)
  (lsp-perfer-flymake nil)
  (lsp-enable-eldoc nil)
  (lsp-message-project-root-warning t)
  :hook ((python-mode . lsp-deferred)
         (lsp-mode . lsp-enable-which-key-integration)))

(use-package lsp-ui
  :diminish lsp-ui-mode
  :commands lsp-ui-mode
  :hook (lsp . lsp-ui)
  :custom-face
  (lsp-ui-doc-background ((t (:background nil))))
  (lsp-ui-doc-header ((t (:inherit (font-lock-string-face italic)))))
  :config
  (setq lsp-ui-sideline-enable nil
        lsp-enable-completion-at-point t
        lsp-ui-doc-position 'at-point
        lsp-ui-doc-header t
        lsp-ui-doc-border (face-foreground 'default)
        lsp-ui-doc-include-signature t))

(use-package lsp-ivy :commands lsp-ivy-workspace-symbol)
(use-package lsp-treemacs :commands lsp-treemacs-errors-list)

(use-package dap-mode
  :config
  (use-package dap-python))

(provide 'modules-lsp)
