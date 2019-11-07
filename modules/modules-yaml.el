(use-package yaml-mode
  :commands (yaml-mode)
  :mode (("\\.yaml\\'" . yaml-mode)
         ("\\.yml\\'" . yaml-mode)))

(provide 'modules-yaml)
