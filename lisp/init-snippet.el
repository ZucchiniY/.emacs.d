;; init-snippet.el --- Initialize snippet configurations.	-*- lexical-binding: t -*-

;; Author: Dylan Yang
;; URL: https://github.com/zucchiniy/.emacs.d

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
;; Snippet configurations.
;;

;;; Code:

;; Yet another snippet extension
(use-package yasnippet
  :diminish yas-minor-mode
  :hook (after-init . yas-global-mode)
  :bind ("C-c y" . yasnippet-hydra/body)
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

(use-package yasnippet-snippets)

(use-package yasnippet-capf
  :after cape
  :commands yasnippet-capf
  :functions cape-capf-super eglot-completion-at-point my-eglot-capf-with-yasnippet
  :init
  (add-to-list 'completion-at-point-functions #'yasnippet-capf)

  ;; To integrate `yasnippet-capf' with `eglot' completion
  ;; https://github.com/minad/corfu/wiki#making-a-cape-super-capf-for-eglot
  (defun my-eglot-capf-with-yasnippet ()
    (setq-local completion-at-point-functions
                (list
	             (cape-capf-super
		          #'eglot-completion-at-point
		          #'yasnippet-capf))))
  (add-hook 'eglot-managed-mode-hook #'my-eglot-capf-with-yasnippet))

(provide 'init-snippet)
;;; init-snippet.el ends here
