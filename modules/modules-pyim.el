(use-package pyim
  :demand t
  :diminish pyim-isearch-mode
  :init
  (setq default-input-method "pyim"
        pyim-default-scheme 'rime
        pyim-title "ㄓ"
        pyim-page-length 7
        pyim-page-tooltip 'proframe)
  :config
  (setq-default pyim-english-input-switch-functions
                '(pyim-probe-dynamic-english
                  pyim-probe-evil-normal-mode
                  pyim-probe-program-mode
                  pyim-probe-org-structure-template))

  (setq-default pyim-punctuation-half-width-functions
                '(pyim-probe-punctuation-line-beginning
                  pyim-probe-punctuation-after-punctuation))
  (pyim-isearch-mode t)
  :bind ("M-j" . pyim-convert-string-at-point))

(use-package liberime
  :load-path (lambda () (expand-file-name "rime" user-emacs-directory))
  :custom
  (rime_share_data_dir "/Library/Input Methods/Squirrel.app/Contents/SharedSupport/")
  (rime_user_data_dir (expand-file-name "rime" user-emacs-directory))
  :init
  (module-load (expand-file-name "liberime.so" user-emacs-directory))
  :config
  (liberime-start rime_share_data_dir rime_user_data_dir)
  (liberime-select-schema "wubi86"))

(use-package posframe)

(provide 'modules-pyim)
