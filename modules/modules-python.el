(use-package anaconda-mode
  :commands anaconda-mode
  :general
  (global-leader
    "pd" 'anaconda-mode-find-definitions)
  :init
  (add-hook 'python-mode-hook #'anaconda-mode)
  (add-hook 'python-mode-hook #'anaconda-eldoc-mode))

(use-package company-anaconda
  :after company
  :init
  (add-to-list 'company-backends '(company-anaconda :with company-capf)))

(use-package quickrun
  :general
  (global-leader
    "x" 'quickrun))

(provide 'modules-python)
