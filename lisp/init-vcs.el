;;; init-vcs.el --- summary -*- lexical-binding: t -*-

;; Copyright (C) 2019-2026 Dylan Yang

;; Author: Dylan Yang <banshiliuli1990@sina.com>
;; URL: https://github.com/zucchiniy/.emacs.d

;;; Commentary:

;;; Code:
(eval-when-compile
  (require 'init-const))

(use-package magit
  :init
  (setq magit-diff-refine-hunk t
        git-commit-major 'git-commit-elisp-text-mode)
  :general
  (global-leader
    "g g" 'magit-status
    "g ." 'magit-dispatch))

(use-package magit-prime
  :diminish
  :hook after-init)

(use-package magit-todos
  :after magit-status
  :commands magit-todos-mode
  :hook (magit-status-mode . magit-todos-mode)
  :init
  (setq magit-todos-nice (if (executable-find "nice") t nil))
  (magit-todos-mode 1))

;; Walk through git revisions of a file
(use-package git-timemachine
  :bind (:map vc-prefix-map
              ("t" . git-timemachine)))

(provide 'init-vcs)

;;; init-vcs.el ends here
