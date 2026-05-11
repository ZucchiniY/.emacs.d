;; init-edit.el --- Initialize editing configurations.	-*- lexical-binding: t -*-

;; Author: Dylan Yang
;; URL: https://github.com/zucchiniy/.emacs.d

;;; Commentary:
;;
;; Editing configurations.
;;

;;; Code:
(eval-when-compile
  (require 'init-const))

(use-package delsel
  :ensure nil
  :hook (after-init . delete-selection-mode))

(use-package autorevert
  :ensure nil
  :diminish
  :hook (after-init . global-auto-revert-mode))

(use-package avy
  :bind (("C-:"   . avy-goto-char)
         ("C-'"   . avy-goto-char-2)
         ("M-g l" . avy-goto-line)
         ("M-g w" . avy-goto-word-1))
  :hook (after-init . avy-setup-default)
  :config (setq avy-all-windows nil
                avy-all-windows-alt t
                avy-background t
                avy-style 'pre))

(use-package ace-link
  :bind ("M-o" . ace-link-addr)
  :hook (after-init . ace-link-setup-default))

(use-package aggressive-indent
  :diminish
  :functions too-long-file-p
  :autoload aggressive-indent-mode
  :hook ((after-init . global-aggressive-indent-mode)
         (find-file . (lambda ()
                        (when (too-long-file-p)
                          (aggressive-indent-mode -1)))))
  :config
  (dolist (mode '(gitconfig-mode asm-mode web-mode html-mode
                  css-mode css-ts-mode go-mode go-ts-mode
                  python-ts-mode yaml-ts-mode scala-mode
                  shell-mode term-mode vterm-mode))
    (add-to-list 'aggressive-indent-excluded-modes mode)))

(use-package anzu
  :diminish
  :bind (([remap query-replace] . anzu-query-replace)
         ([remap query-replace-regexp] . anzu-query-replace-regexp)
         :map isearch-mode-map
         ([remap isearch-query-replace] . anzu-isearch-query-replace)
         ([remap isearch-query-replace-regexp] . anzu-isearch-query-replace-regexp))
  :hook (after-init . global-anzu-mode))

(use-package ediff
  :ensure nil
  :hook(;; show org ediffs unfolded
        (ediff-prepare-buffer . outline-show-all)
        ;; restore window layout when done
        (ediff-quit . winner-undo))
  :config
  (setq ediff-window-setup-function 'ediff-setup-windows-plain
        ediff-split-window-function 'split-window-horizontally
        ediff-merge-split-window-function 'split-window-horizontally))

(use-package iedit
  :bind (:map global-map
         ("C-;" . iedit-mode)
         ("C-x r RET" . iedit-rectangle-mode)
         :map isearch-mode-map
         ("C-;" . iedit-mode-from-isearch)
         :map esc-map
         ("C-;" . iedit-execute-last-modification)
         :map help-map
         ("C-;" . iedit-mode-toggle-on-function)))

(use-package expand-region
  :bind ("C-=" . er/expand-region))

(use-package flyspell
  :ensure nil
  :diminish
  :if (executable-find "aspell")
  :hook ((text-mode outline-mode)
         (prog-mode . flyspell-prog-mode))
  :init (setq flyspell-issue-message-flag nil
              flyspell-issue-welcome-flag nil
              ispell-program-name "aspell"
              ispell-extra-args '("--sug-mode=ultra" "--lang=en_US" "--run-together")))

(use-package hungry-delete
  :diminish
  :hook (after-init . global-hungry-delete-mode)
  :init (setq hungry-delete-chars-to-skip " \t\f\v"
              hungry-delete-except-modes
              '(help-mode minibuffer-mode minibuffer-inactive-mode calc-mode)))

(use-package mwim
  :bind (([remap move-beginning-of-line] . mwim-beginning)
         ([remap move-end-of-line] . mwim-end)))

(use-package subword
  :ensure nil
  :diminish
  :hook (prog-mode minibuffer-setup))

(unless sys/winntp
  (use-package xclip
    :hook (after-init . xclip-mode)))

(unless sys/winntp
  (use-package sudo-edit))

(use-package so-long
  :hook (after-init . global-so-long-mode))

(provide 'init-edit)
;;; init-edit.el ends here
