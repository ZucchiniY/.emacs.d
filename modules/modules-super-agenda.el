(use-package org-super-agenda
  :diminish
  :hook (org-agenda-mode . org-super-agenda-mode)
  :config
  (setq org-super-agenda-groups
        '(
          (:name "Today"
                 :time-grid t
                 :todo "TODAY")
          (:name "Important"
                 :tag ("Develop" "Blog")
                 :priority "A")
          (:name "Personal"
                 :habit t
                 :tag "personal")
          (:priority<= "B"
                       :order 1))))

(provide 'modules-super-agenda)
