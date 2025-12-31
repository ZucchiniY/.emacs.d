;;; package --- Summary
;;; Commentary:
;;; config lsp mode
;;; Code:
;; lsp-python
(use-package eglot
  :hook ((prog-mode . (lambda ()
                        (unless (derived-mode-p
                                 'emacs-lisp-mode 'lisp-mode
                                 'makefile-mode 'snippet-mode
                                 'ron-mode)
                          (eglot-ensure))))
         ((markdown-mode yaml-mode yaml-ts-mode) . eglot-ensure)
         ((c-ts-mode c-mode) . eglot-ensure)
         ((c++-mode c++-ts-mode) . eglot-ensure)
         ((c-or-c++-mode c-or-c++-ts-mode) . eglot-ensure)
         ((python-mode python-ts-mode) . eglot-ensure)
         ((rust-mode rust-ts-mode) . eglot-ensure))
  :init
  (setq eglot-autoshutdown t
        eglot-events-buffer-size 0
        eglot-send-changes-idle-time 0.5)
  :config
  (when (fboundp 'eglot-ensure)
    (eglot-ensure))
  (add-to-list 'eglot-stay-out-of 'flymake)
  :general
  (general-define-key
   :states 'normal
   :keymaps 'override
   :prefix "SPC e"
   "d" 'xref-find-definitions
   "r" 'eglot-rename
   "h" 'eldoc
   "R" 'eglot-reconnect
   "f" 'eglot-format
   "ca" 'eglot-code-actions
   "cq" 'eglot-code-action-quickfix
   "ci" 'eglot-code-action-inline
   "cr" 'eglot-code-action-rewrite
   "co" 'eglot-code-action-organize-imports
   )
  )

;; use emacs-lsp-boost to speed up eglot
(use-package eglot-booster
  :ensure nil ;; use built-in eglot
  :after eglot
  :config (eglot-booster-mode))
;; (use-package lspce
;;   :load-path "load-lisp/lspce"
;;   :hook ((rust-mode . lspce-mode)
;;          (c-mode. lspce-mode)
;;          (python-mode . lspce-mode))
;;   :config
;;   (progn
;;     (setq lspce-send-changed-idle-time 0.1
;;           lspce-idle-delay 0.1)
;;     (setq lspce-show-log-level-in-modeline t
;;           lspce-eldoc-enable-hover nil
;;           lspce-eldoc-enable-signature t)
;;     (setq eldoc-idle-delay 0.1)
;;     ;; (add-hook 'lspce-mode-hook #'lspce-inlay-hints-mode)
;;     (lspce-set-log-file "~/.emacs.d/lspce.log")
;;     (lspce-enable-logging)
;;     (setq lspce-sever-programs
;;           `(("rust" "rust-analyzer" "" lspce-ra-initializationOptions)
;;             ("C" "clangd" "--all-scopes-completion --clang-tidy --enable-config --header-insertion-decorators=0")
;;             ("typescript" "deno" "" lspce-deno-initializationOptions)
;;             ("javascript" "deno" "" lspce-deno-initializationOptions)
;;             ("python" "ruff" "" lspce-ruff-initializationOptions)))
;;     :general
;;     (general-define-key
;;      :states '(normal visual emacs)
;;      :keymaps 'override
;;      :prefix "SPC l"
;;      "d" 'xref-find-definitions
;;      "r" 'xref-find-references
;;      "a" 'lspce-code-actions)
;;     ))

(provide 'init-lsp)
;;; init-lsp.el ends here
