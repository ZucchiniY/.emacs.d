;;; dylan-web.el --- summary -*- lexical-binding: t -*-

;; Author: Dylan Yang
;; Maintainer: Dylan Yang

;;; Commentary:

;;; Code:

(use-package css-mode
  :ensure nil
  :init (setq css-indent-offset 2))

(use-package scss-mode
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
  :mode "\\.\\(html\\|vue\\|js\\)$"
  :config
  (setq web-mode-markup-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2))

(use-package company-web
  :after web-mode
  :hook (web-mode . company-mode))

(use-package emmet-mode
  :hook ((css-mode . emmet-mode)
         (js2-mode . emmet-mode)))

(use-package vue-mode
  :mode ("\\.vue\\'" . vue-mode)
  :config (setq mmm-submode-decoration-level 0))

(provide 'dylan-web)

;;; dylan-web.el ends here
