;; core-keybind.el --- config evil and evil keybind.	-*- lexical-binding: t -*-

;; Copyright (C) 2019-2021 Dylan Yang

;; Author: Dylan Yang <banshiliuli1990@sina.com>
;; URL: https://github.com/zucchiniy/.emacs.d

;;; Commentary:
;;

;;; Code:
(use-package general
  :defer 1
  :commands general-override-states
  :config
  (general-evil-setup t)
  (general-create-definer global-leader
    :states '(normal visual motion)
    :prefix "SPC"
    :keymaps 'override)
  (general-create-definer local-leader
    :states '(normal visual motion)
    :prefix "SPC m"
    :keymaps 'override)
  (global-leader
    ;; files keybinds
    "fs" 'save-buffer
    "fd" 'dired
    ;; buffer keybinds
    "bq" 'kill-buffer
    ;; quite emacs
    "qq" 'save-buffers-kill-emacs
    ;; winner
    "wu" 'winner-undo
    "wr" 'winner-redo
    ;; window keybinds
    "wo" 'other-window
    "wv" 'split-window-vertically
    "w-" 'split-window-horizontally
    "wl" 'evil-window-right
    "wh" 'evil-window-left
    "wk" 'evil-window-up
    "wj" 'evil-window-down
    "wq" 'delete-window
    "wa" 'delete-other-windows
    ;; windows select
     "1" 'winum-select-window-1
     "2" 'winum-select-window-2
     "3" 'winum-select-window-3
     "4" 'winum-select-window-4
     "5" 'winum-select-window-5
     "6" 'winum-select-window-6
     "7" 'winum-select-window-7
     "8" 'winum-select-window-8
     "9" 'winum-select-window-9
     "0" 'winum-select-window-0-or-10
     ;; cfw::open-org-calendar
     "ov" 'cfw:open-org-calendar
    ))

(use-package evil
  :defer 1
  :hook (after-init . evil-mode)
  :init
  (setq evil-want-keybinding nil
        evil-want-C-i-jump nil
        evil-want-integration t)
  :config
  (setq evil-default-state 'normal)
  ;; 将不希望使用 Evil Mode 的内容，放在这里
  (evil-set-initial-state 'dired-mode 'emacs)
  (evil-set-initial-state 'deft-mode 'emacs)
  ;; evil ex command `:W' to save all buffers.
  (evil-ex-define-cmd "W" 'evil-write-all)
  
  ;; evil normal state keybinds
  (define-key evil-normal-state-map "Y" (kbd "y$")))

;; add package evil-collection
(use-package evil-collection
  :defer 1
  :after evil
  :config
  (evil-collection-init)
  (setq forge-add-default-bindings nil)
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

(provide 'core-keybind)
;;; core-keybind.el ends here

