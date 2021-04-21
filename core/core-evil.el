(use-package evil
  :hook (after-init . evil-mode)
  :init (setq evil-want-keybinding nil)
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
  :config (evil-collection-init))

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
  :init (global-evil-mc-mode 1))

(use-package evil-mc-extras
  :after evil-mc
  :diminish evil-mc-extras-mode)

(provide 'core-evil)
