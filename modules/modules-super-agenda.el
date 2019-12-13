(use-package org-super-agenda
  :diminish
  :hook (org-agenda-mode . org-super-agenda-mode)
  :config
  (setq org-super-agenda-groups
        '(
          (:name "当天"
                 :time-grid t)
          (:name "重要且紧急"
                 :and (:todo "IMPORTANT"
                             :priority "A"))
          (:name "重要不紧急"
                 :and (:todo "IMPORTANT"
                             :priority<= "B"))
          (:name "紧急不重要"
                 :and (:priority "A"
                                 :not (:todo "IMPORTANT")))
          (:name "阅读"
                 :tag "Reading")
          (:name "习惯养成"
                 :habit t)
          (:discard (:anything t)))))

(provide 'modules-super-agenda)
