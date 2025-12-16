;; core-package.el --- Define Package config.	-*- lexical-binding: t -*-

;; Copyright (C) 2019-2021 Dylan Yang

;; Author: Dylan Yang <banshiliuli1990@sina.com>
;; URL: https://github.com/zucchiniy/.emacs.d

;;; Commentary:
;;

;;; Code:
;; Set package archives and use-package.
(require 'core-basis)

;; (setq package-archives ustc-elpa)
(setq package-archives tuna-elpa)

;; Initialize packages
(unless (bound-and-true-p package--initialized)
  (setq package-enable-at-startup nil)
  (package-initialize))

;; use package
;; if emacs-version < 29.1 load use-package
;; else load use-package from load-lisp/
(if (version< emacs-version "29.1")
    (push (expand-file-name "load-lisp/use-package" user-emacs-directory) load-path)
  ;; emacs 29.1 update build-in package auto
  (setq package-install-upgrade-built-in t))

(eval-when-compile
  (require 'use-package))

;; add hydra package
(use-package hydra :ensure t)
(use-package use-package-hydra :ensure t)

;; add diminish
(use-package diminish :ensure t)

(setq use-package-always-ensure t)
(setq use-package-always-defer t)
(setq use-package-expand-minimally t)
(setq use-package-enable-imenu-support t)

;; abbrev-mode abbreviation file-name
(use-package abbrev
  :ensure nil
  :diminish abbrev-mode
  :config
  (if (file-exists-p abbrev-file-name)
      (quietly-read-abbrev-file)))

;; use package-utils to update packages
(use-package package-utils
  :init
  (defalias 'upgrade-packages 'package-utils-upgrade-all)
  (defalias 'upgrade-packages-and-restart 'package-utils-upgrade-all-and-restart))

(use-package no-littering
  :init
  (setq no-littering-etc-directory (expand-file-name "config/" user-emacs-directory)
        no-littering-var-directory (expand-file-name "data/" user-emacs-directory)))

(use-package which-key
  :diminish which-key-mode
  :hook (after-init . which-key-mode)
  :config (which-key-mode))

(use-package htmlize :defer t)

(use-package undo-tree
  :ensure t
  :init (global-undo-tree-mode 1)
  :config
  (setq undo-tree-auto-save-history nil)
  :after hydra
  :bind ("<f6>" . hydra-undo-tree/body)
  :hydra (hydra-undo-tree (:hint nil)
                          "
  _p_: undo  _n_: redo _s_: save _l_: load   "
  ("p"   undo-tree-undo)
  ("n"   undo-tree-redo)
  ("s"   undo-tree-save-history)
  ("l"   undo-tree-load-history)
  ("u"   undo-tree-visualize "visualize" :color blue)
  ("q"   nil "quit" :color blue)))

(use-package multiple-cursors
  :ensure t
  :after hydra
  :bind
  ("<f7>" . hydra-multiple-cursors/body)
  :hydra (hydra-multiple-cursors
		  (:hint nil)
		  "
Up^^             Down^^           Miscellaneous           % 2(mc/num-cursors) cursor%s(if (> (mc/num-cursors) 1) \"s\" \"\")
------------------------------------------------------------------
 [_p_]   Prev     [_n_]   Next     [_l_] Edit lines  [_0_] Insert numbers
 [_P_]   Skip     [_N_]   Skip     [_a_] Mark all    [_A_] Insert letters
 [_M-p_] Unmark   [_M-n_] Unmark   [_s_] Search      [_q_] Quit
 [_|_] Align with input CHAR       [Click] Cursor at point"
		  ("l" mc/edit-lines :exit t)
		  ("a" mc/mark-all-like-this :exit t)
		  ("n" mc/mark-next-like-this)
		  ("N" mc/skip-to-next-like-this)
		  ("M-n" mc/unmark-next-like-this)
		  ("p" mc/mark-previous-like-this)
		  ("P" mc/skip-to-previous-like-this)
		  ("M-p" mc/unmark-previous-like-this)
		  ("|" mc/vertical-align)
		  ("s" mc/mark-all-in-region-regexp :exit t)
		  ("0" mc/insert-numbers :exit t)
		  ("A" mc/insert-letters :exit t)
		  ("<mouse-1>" mc/add-cursor-on-click)
		  ;; Help with click recognition in this hydra
		  ("<down-mouse-1>" ignore)
		  ("<drag-mouse-1>" ignore)
		  ("q" nil)))

(use-package yasnippet
  :diminish yas-minor-mode
  :hook (after-init . yas-global-mode)
  :config
  (use-package yasnippet-snippets)
  :bind (:map yas-minor-mode-map ("<f2>" . hydra-yas/body))
  :hydra (hydra-yas (:color blue :hint nil)
                    "
              ^YASnippets^
--------------------------------------------
  Modes:    Load/Visit:    Actions:

 _g_lobal  _d_irectory    _i_nsert
 _m_inor   _f_ile         _t_ryout
 _e_xtra   _l_ist         _n_ew
         _a_ll
"
          ("d" yas-load-directory)
          ("e" yas-activate-extra-mode)
          ("i" yas-insert-snippet)
          ("f" yas-visit-snippet-file :color blue)
          ("n" yas-new-snippet)
          ("t" yas-tryout-snippet)
          ("l" yas-describe-tables)
          ("g" yas/global-mode)
          ("m" yas/minor-mode)
          ("a" yas-reload-all)))


(use-package expand-region
  :bind ("C-=" . er/expand-region))

(use-package smart-region
  :hook (after-init . smart-region-on))

(declare-function upgrade-packages-and-restart 'init-package)
(defalias 'dylan-update-packages-and-restartup 'upgrade-packages-and-restart)

(use-package recentf
  :defer t
  :config
  (recentf-mode t))

(use-package helpful
  :config
  (global-set-key (kbd "C-h f") #'helpful-callable)
  (global-set-key (kbd "C-h v") #'helpful-variable)
  (global-set-key (kbd "C-h k") #'helpful-key)
  (global-set-key (kbd "C-h x") #'helpful-command)
  (global-set-key (kbd "C-c C-d") #'helpful-at-point)
  (global-set-key (kbd "C-h F") #'helpful-function))

(use-package markdown-mode :ensure nil)

(provide 'core-package)
;;; core-package.el ends here
