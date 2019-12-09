(use-package wakatime-mode
  :diminish 'wakatime-mode
  :hook (after-init . global-wakatime-mode)
  :config
  (setq wakatime-cli-path "/usr/local/bin/wakatime")
  (setq wakatime-python-bin nil)
  (global-wakatime-mode)
  (setq wakatime-api-key "012375b6-91ca-47ff-be00-3fc48e6bc0b7"))

(provide 'modules-wakatime)
