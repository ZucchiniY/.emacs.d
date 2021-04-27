;; core-evil.el --- config evil and evil keybind.	-*- lexical-binding: t -*-

;; Copyright (C) 2019-2021 Dylan Yang

;; Author: Dylan Yang <banshiliuli1990@sina.com>
;; URL: https://github.com/zucchiniy/.emacs.d

;;; Commentary:
;;

;;; Code:
(use-package evil
  :hook (after-init . evil-mode)
  :init
  (setq evil-want-keybinding nil
        evil-want-integration t)
  :config
  (setq evil-default-state 'normal)
  ;; 将不希望使用 Evil Mode 的内容，放在这里
  (evil-set-initial-state 'dired-mode 'emacs)
  (evil-set-initial-state 'deft-mode 'emacs)
  (evil-set-initial-state 'elfeed-search-mode 'emacs)
  (evil-set-initial-state 'elfeed-show-mode 'emacs)
  ;; evil ex command `:W' to save all buffers.
  (evil-ex-define-cmd "W" 'evil-write-all)
  
  ;; evil normal state keybinds
  (define-key evil-normal-state-map "Y" (kbd "y$")))

;; add package evil-collection
(use-package evil-collection
  :after evil
  :config (evil-collection-init)
  :custom (evil-collection-setup-minibuffer t))

;; gcc comments out a line
;; gc comments out the target of a motion
;; gcap comments out a paragraph
(use-package evil-commentary
  :after evil
  :diminish evil-commentary-mode
  :init (evil-commentary-mode))

;; evil surround
(use-package evil-surround
  :after evil
  :init (global-evil-surround-mode))

(use-package evil-matchit
  :after evil
  :diminish evil-matchit-mode
  :config (global-evil-matchit-mode t))

(use-package evil-mc
  :after evil
  :diminish evil-mc-mode
  :hook (after-init . global-evil-mc-mode)
  :commands (evil-mc-make-cursor-move-next-line
             evil-mc-make-cursor-move-prev-line
             evil-mc-mode
             evil-mc-undo-all-cursors
             global-evil-mc-mode)
  :init (global-evil-mc-mode 1)
  :general
  (general-define-key
   :states 'normal
   :keymaps 'override
   :prefix "SPC r"
    "m" 'evil-mc-make-all-cursors
    "u" 'evil-mc-undo-last-added-cursor
    "q" 'evil-mc-undo-all-cursors
    "s" 'evil-mc-pause-cursors
    "r" 'evil-mc-resume-cursors
    "f" 'evil-mc-make-and-goto-first-cursor
    "l" 'evil-mc-make-and-goto-last-cursor
    "h" 'evil-mc-make-cursor-here
    "j" 'evil-mc-make-cursor-move-next-line
    "k" 'evil-mc-make-cursor-move-prev-line
    "N" 'evil-mc-skip-and-goto-next-cursor
    "P" 'evil-mc-skip-and-goto-prev-cursor
    "n" 'evil-mc-skip-and-goto-next-match
    "p" 'evil-mc-skip-and-goto-prev-match
    "I" 'evil-mc-make-cursor-in-visual-selection-beg
    "A" 'evil-mc-make-cursor-in-visual-selection-end
    "M-n" 'evil-mc-make-and-goto-next-cursor
    "M-p" 'evil-mc-make-and-goto-prev-cursor
    "C-n" 'evil-mc-make-and-goto-next-match
    "C-t" 'evil-mc-skip-and-goto-next-match
    "C-p" 'evil-mc-make-and-goto-prev-match
    "+" 'evil-mc-inc-num-at-each-cursor
    "-" 'evil-mc-dec-num-at-each-cursor
   ))

(use-package evil-mc-extras
  :after evil-mc
  :commands global-evil-mc-extras-mode
  :diminish evil-mc-extras-mode
  :init (global-evil-mc-extras-mode 1))

(provide 'core-evil)
;;; core-evil.el ends here

