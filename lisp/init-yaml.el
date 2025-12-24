;;; init-yaml.el --- summary -*- lexical-binding: t -*-

;; Author: Dylan Yang
;; Maintainer: Dylan Yang
;; Version: 1.0.0
;; Package-Requires: ()
;;; Commentary:

;;; Code:

(use-package yaml-mode
  :commands (yaml-mode)
  :mode (("\\.yaml\\'" . yaml-mode)
         ("\\.yml\\'" . yaml-mode)))

(provide 'init-yaml)

;;; init-yaml.el ends here
