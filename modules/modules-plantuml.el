(use-package plantuml-mode
  :mode ("\\.plantuml\\'" . plantuml-mode)
  :defer t
  :config
  (setq plantuml-jar-path (expand-file-name "extends/plantuml.jar" user-emacs-directory)))

(use-package flycheck-plantuml
  :after plantuml-mode
  :config (flycheck-plantuml-setup))

(provide 'modules-plantuml)
