(use-package writeroom-mode
  :init
  (setq writeroom-extra-line-spacing 5
        writeroom-width 0.5)
  (advice-add 'writeroom--calculate-width :before #'redisplay))

(provide 'modules-writer)
