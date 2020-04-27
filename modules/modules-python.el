(use-package anaconda-mode
  :commands anaconda-mode
  :diminish (anaconda-eldoc-mode anaconda-mode)
  :general
  (global-leader
    "pd" 'anaconda-mode-find-definitions)
  :init
  (add-hook 'python-mode-hook #'anaconda-mode))

(use-package company-anaconda
  :after company
  :init
  (add-to-list 'company-backends '(company-anaconda :with company-capf)))

(provide 'modules-python)
