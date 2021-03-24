;;; package --- Summary
;;; Commentary:
;;; Use lsp-mode for Emacs client
;;; Code:
(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c l")
  :hook (
         (python-mode . lsp-deferred)
         (vue-mode .lsp-deferred)
         (lsp-mode . lsp-enable-which-key-integration)
         )
  :commands (lsp lsp-deferred))

(use-package lsp-ui
  :diminish lsp-ui-mode
  :commands lsp-ui-mode
  :bind (("C-c u" . lsp-ui-mode)
         :map lsp-ui-mode-map
         ("M-<f6>" . lsp-ui-hydra/body)
         ("M-RET" . lsp-ui-sideline-apply-code-actions))
  :hook (lsp-mode . lsp-ui-mode))

(use-package lsp-ivy
  :commands lsp-ivy-workspace-symbol)
(use-package lsp-treemacs
  :commands lsp-treemacs-errors-list)

(use-package dap-mode
  :defines dap-python-executable
  :functions dap-hydra/nil
  :diminish
  :bind (:map lsp-mode-map
              ("<f5>" . dap-debug)
              ("M-<f5>" . dap-hydra))
  :hook ((after-init . dap-auto-configure-mode)
         (dap-stopped . (lambda (_args) (dap-hydra)))
         (python-mode . (lambda () (require 'dap-python)))
         (vue-mode . (lambda () (require 'dap-vue)))
         )
  :init
  (when (executable-find "python3")
    (setq dap-python-executable "python3")))
                         

(use-package lsp-pyright
  :preface
  (defun lsp-pyright-format-buffer ()
    (interactive)
    (when (and (executable-find "yapf") buffer-file-name)
      (call-process "yapf" nil nil nil "-i" buffer-file-name)))
  :hook (python-mode . (lambda ()
                         (require 'lsp-pyright)
                         (add-hook 'after-save-hook #'lsp-pyright-format-buffer t t)))
  :init (when (executable-find "python3")
          (setq lsp-pyright-python-executable-cmd "python3")))

(provide 'modules-lsp)
