;;; modules-rust.el --- summary -*- lexical-binding: t -*-

;; Author: Dylan Yang
;; Maintainer: Dylan Yang
;; Version: 0.1.0
;; Package-Requires: (rust-analyzer)
;; Homepage: homepage
;; Keywords: keywords


;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.


;;; Commentary:

;; commentary

;;; Code:

(use-package rust-mode
  :mode ("\\.rs\\'" . rust-mode)
  :init (setq rust-format-on-save t
              rust-mode-treesitter-derive t)
  :config
  (add-hook 'rust-mode-hook (lambda () (setq indent-tabs-mode nil)))
  (add-hook 'flycheck-mode-hook 'flycheck-rust-setup)
  (define-derived-mode rustic-mode rust-mode "Rust"
    "Major mode for Rust code."))

(use-package flycheck-rust :ensure t
  :config (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

(use-package toml-mode)
(use-package cargo)
(use-package ron-mode
  :mode ("\\.ron" . ron-mode))

(provide 'modules-rust)

;;; modules-rust.el ends here
