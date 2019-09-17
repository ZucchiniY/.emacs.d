;;; dylan-expansion.el --- summary -*- lexical-binding: t -*-

;; Author: Dylan Yang
;; Maintainer: Dylan Yang

;;; Commentary:

;;; Code:

;; Emacs development
(use-package elisp-mode
  :ensure nil
  :bind (:map emacs-lisp-mode-map
              ("C-c C-x" . ielm)
              ("C-c C-c" . eval-defun)
              ("C-c C-b" . eval-buffer)))

(use-package eldoc
  :ensure nil
  :diminish eldoc-mode)

;; Emacs reading
(use-package elfeed
  :bind ("C-x w e" . elfeed)
  :config
  (setq elfeed-db-directory (expand-file-name ".elfeed" user-emacs-directory)
        elfeed-feeds
        '("http://planet.emacsen.org/atom.xml"
          "http://www.masteringemacs.org/feed/"
          "https://oremacs.com/atom.xml"
          "https://pinecast.com/feed/emacscast")))

;; Flycheck
(use-package flycheck
  :diminish flycheck-mode
  :hook (after-init . global-flycheck-mode)
  :config
  (setq flycheck-indication-mode 'right-fringe
        flycheck-emacs-lisp-load-path 'inherit)
  (flycheck-add-mode 'javascript-eslint 'web-mode))

(provide 'dylan-expansion)
;;; dylan-expansion.el ends here
