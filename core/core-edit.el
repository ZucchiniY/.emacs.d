;; core-edit.el --- Initialize editing configurations.	-*- lexical-binding: t -*-

;; Copyright (C) 2006-2025 Vincent Zhang

;; Author: Vincent Zhang <seagle0128@gmail.com>
;; URL: https://github.com/seagle0128/.emacs.d

;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;

;;; Commentary:
;;
;; Editing configurations.
;;

;;; Code:
(eval-when-compile
  (require 'core-const)
  (require 'core-funcs))

(use-package which-key
  :diminish which-key-mode
  :hook (after-init . which-key-mode)
  :config (which-key-mode))

(use-package htmlize)

(use-package undo-tree
  :init (global-undo-tree-mode 1)
  :config
  (setq undo-tree-auto-save-history nil)
  :after hydra
  :bind ("<f8>" . undo-tree-hydra/body)
  :pretty-hydra
  ((:title (pretty-hydra-title "undo-tree" 'faicon "nf-fa-undo" :face 'nerd-icons-orange)
           :color amaranth :quit-key ("q" "C-g"))
   ("Undo"
    (("p" undo-tree-undo "undo")
     ("n" undo-tree-redo "redo")
     ("s" undo-tree-save-history "save history")
     ("l" undo-tree-load-history "load history")
     ("u" undo-tree-visualize "visualize" :color blue "visualize-tree")
     ("q" nil "quit" :color blue)))))

(use-package multiple-cursors
  :after hydra
  :bind
  ("<f7>" . multiple-cursors-hydra/body)
  :pretty-hydra
  ((:title (pretty-hydra-title "multiple-cursors" 'faicon "nf-fa-i_cursor" :face 'nerd-icons-orange)
           :color amaranth :quit-key ("q" "C-g"))
           ("Miscellaneous"
            (("l" mc/edit-lines "edit lines")
             ("a" mc/mark-all-like-this "mark all")
             ("s" mc/mark-all-in-region-regexp "search")
             ("<mouse-1>" mc/add-cursor-on-click "cursor at point"))
            "Down"
            (("n" mc/mark-next-like-this "next")
            ("N" mc/skip-to-next-like-this "skip to next")
            ("M-n" mc/unmark-next-like-this "Unmark next"))
            "Up"
            (("p" mc/mark-previous-like-this "Previous")
             ("P" mc/skip-to-previous-like-this "Skip Previous")
             ("M-p" mc/unmark-previous-like-this "unmark Previous"))
            "Other"
            (("|" mc/vertical-align "Align with input CHAR")
             ("0" mc/insert-numbers "insert numbers")
             ("A" mc/insert-letters "insert letters")))))

(use-package yasnippet
  :diminish yas-minor-mode
  :hook (after-init . yas-global-mode)
  :config
  (use-package yasnippet-snippets)
  :bind (:map yas-minor-mode-map ("<f2>" . yas-hydra/body))
  :pretty-hydra
  ((:title (pretty-hydra-title "YASnippets" 'faicon "nf-fa-tripadvisor")
           :color amaranth :quit-key ("q" "C-g"))
           ("Modes"
            (("g" yas/global-mode "global")
             ("m" yas/minor-mode "minor")
             ("e" yas-activate-extra-mode "extra"))
            "Load/Visit"
            (("d" yas-load-directory "load directory")
             ("f" yas-visit-snippet-file :color blue "visit snippet file")
             ("l" yas-describe-tables "describe tables"))
            "Actions"
            (("i" yas-insert-snippet "insert snippet")
             ("n" yas-new-snippet "new snippet")
             ("t" yas-tryout-snippet "tryout snippet")
             ("a" yas-reload-all "reload all")))))

(use-package expand-region
  :bind ("C-=" . er/expand-region))

(use-package smart-region
  :hook (after-init . smart-region-on))

(use-package helpful
  :config
  (global-set-key (kbd "C-h f") #'helpful-callable)
  (global-set-key (kbd "C-h v") #'helpful-variable)
  (global-set-key (kbd "C-h k") #'helpful-key)
  (global-set-key (kbd "C-h x") #'helpful-command)
  (global-set-key (kbd "C-c C-d") #'helpful-at-point)
  (global-set-key (kbd "C-h F") #'helpful-function))

(use-package markdown-mode :ensure nil :defer t)

(provide 'core-edit)
;;; core-edit.el ends here
