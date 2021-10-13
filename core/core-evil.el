;; core-evil.el --- config evil and evil keybind.	-*- lexical-binding: t -*-

;; Copyright (C) 2019-2021 Dylan Yang

;; Author: Dylan Yang <banshiliuli1990@sina.com>
;; URL: https://github.com/zucchiniy/.emacs.d

;;; Commentary:
;;

;;; Code:
(use-package evil
  :defer 1
  :hook (after-init . evil-mode)
  :init
  (setq evil-want-keybinding nil
        evil-want-integration t)
  :config
  (setq evil-want-C-i-jump nil)
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
  :defer 1
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
  )

(use-package evil-mc-extras
  :after evil-mc
  :commands global-evil-mc-extras-mode
  :diminish evil-mc-extras-mode
  :init (global-evil-mc-extras-mode 1))

(provide 'core-evil)
;;; core-evil.el ends here

