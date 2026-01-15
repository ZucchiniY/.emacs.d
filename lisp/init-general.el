;; init-general.el --- config evil and evil keybind.	-*- lexical-binding: t -*-

;; Copyright (C) 2019-2026 Dylan Yang

;; Author: Dylan Yang <banshiliuli1990@sina.com>
;; URL: https://github.com/zucchiniy/.emacs.d

;;; Commentary:

;;; Code:
(use-package evil
  :commands (evil-set-initial-state evil-ex-define-cmd)
  :hook (after-init . evil-mode)
  :init
  (setq evil-want-keybinding nil
        evil-want-C-i-jump nil
        evil-want-integration t)
  :config
  (setq evil-default-state 'normal)
  ;; 将不希望使用 Evil Mode 的内容，放在这里
  (evil-set-initial-state 'dired-mode 'emacs)
  (evil-set-initial-state 'dashboard-mode 'emacs)
  (evil-set-initial-state 'deft-mode 'emacs)
  (evil-set-initial-state 'ibuffer-mode 'emacs)
  ;; evil ex command `:W' to save all buffers.
  (evil-ex-define-cmd "W" 'evil-write-all)

  ;; evil normal state keybinds
  (define-key evil-normal-state-map "Y" (kbd "y$")))

;; add package evil-collection
(use-package evil-collection
  :defer t
  :after (evil magit)
  :custom
  (setq evil-collection-outline-bind-tab-p nil
        evil-collection-setup-minibuffer t)
  :config
  (setq evil-collection-mode-list (delete 'company evil-collection-mode-list))
  (evil-collection-init)
  )

;; gcc comments out a line
;; gc comments out the target of a motion
;; gcap comments out a paragraph
(use-package evil-commentary
  :defer t
  :after evil
  :diminish evil-commentary-mode
  :init (evil-commentary-mode))

;; evil surround
(use-package evil-surround
  :defer
  :after evil
  :init (global-evil-surround-mode))

(use-package evil-matchit
  :defer t
  :after evil
  :diminish evil-matchit-mode
  :config (global-evil-matchit-mode t))

(use-package evil-mc
  :defer t
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
  :defer t
  :after evil-mc
  :commands global-evil-mc-extras-mode
  :diminish evil-mc-extras-mode
  :init (global-evil-mc-extras-mode 1))

(use-package general
  :commands (general-override-states global-leader)
  :config
  (general-override-mode)
  (general-evil-setup)
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
    ;; "fs" 'save-buffer
    ;; "fd" 'dired
    ;; buffer keybinds
    ;; "bq" 'kill-buffer
    ;; quite emacs
    "qq" 'save-buffers-kill-emacs
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
    ;; cfw::open-org-calendar
    ;; "ov" 'cfw:open-org-calendar
    ;; "xo" 'xwidget-webkit-browse-url
    ))

(provide 'init-general)
;;; init-general.el ends here
