(use-package css-mode
  :ensure nil
  :init (setq css-indent-offset 2))

(use-package scss-mode
  :defines scss-copile-at-save
  :mode (("\\.scss\\'" . scss-mode)
         ("\\.sass\\'" . scss-mode))
  :hook (scss-mode . rainbow-mode)
  :config (setq scss-copile-at-save nil))

(use-package less-css-mode)

(use-package css-eldoc
  :commands turn-on-css-eldoc
  :hook ((css-mode scss-mode less-css-mode) . turn-on-css-eldoc))

(use-package json-mode)

(use-package web-mode
  :defines company-backends
  :mode "\\.\\(jsx\\|html\\|vue\\|js\\)$"
  :config
  (setq web-mode-markup-indent-offset 4
        web-mode-css-indent-offset 4
        web-mode-code-indent-offset 4))

(use-package company-web
  :after web-mode
  :hook (web-mode . company-mode))

(use-package emmet-mode
  :hook ((css-mode . emmet-mode)
         (js2-mode . emmet-mode)))

(provide 'modules-web)
