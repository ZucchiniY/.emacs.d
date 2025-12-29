;; init-custom.el --- Define customizations.	-*- lexical-binding: t -*-

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
;; Customization.
;;

;;; Code:

(eval-when-compile
  (require 'package))

(defgroup dylan nil
  "Dylan Emacs customization."
  :group 'convenience
  :link '(url-link :tag "Homepage" "https://github.com/zucchiniy/.emacs.d"))

(defcustom dylan-logo (expand-file-name
                         (if (display-graphic-p) "logo.png" "banner.txt")
                         user-emacs-directory)
  "Set Dylan logo. nil means official logo."
  :group 'dylan
  :type 'string)

(defcustom dylan-full-name user-full-name
  "Set user full name."
  :group 'dylan
  :type 'string)

(defcustom dylan-mail-address user-mail-address
  "Set user email address."
  :group 'dylan
  :type 'string)

(defcustom dylan-org-directory (expand-file-name "~/org")
  "Set org directory."
  :group 'dylan
  :type 'string)

(defcustom dylan-org-roam-directory (expand-file-name "roam")
  "Set org roam directory."
  :group 'dylan
  :type 'string)

(defcustom dylan-server t
  "Enable `server-mode' or not."
  :group 'dylan
  :type 'boolean)

(defcustom dylan-use-exec-path-from-shell
  (or (memq window-system '(mac ns x)) (daemonp))
  "Use `exec-path-from-shell' or not.
If using emacs-plus with path ejection, set to nil."
  :group 'dylan
  :type 'boolean)

(defcustom dylan-icon t
  "Display icons or not."
  :group 'dylan
  :type 'boolean)

(defcustom dylan-completion-style 'childframe
  "Completion display style."
  :group 'dylan
  :type '(choice (const :tag "Minibuffer" minibuffer)
                 (const :tag "Child Frame" childframe)))

(defcustom dylan-frame-maximized-on-startup nil
  "Maximize frame on startup or not."
  :group 'dylan
  :type 'boolean)

(defcustom dylan-dashboard (not (daemonp))
  "Display dashboard at startup or not.
If Non-nil, use dashboard, otherwise will restore previous session."
  :group 'dylan
  :type 'boolean)

(defcustom dylan-tree-sitter t
  "Enable tree-sitter or not.
Native tree-sitter is introduced in 29."
  :group 'dylan
  :type 'boolean)

(defcustom dylan-lsp-format-on-save nil
  "Auto format buffers on save."
  :group 'dylan
  :type 'boolean)

(defcustom dylan-lsp-format-on-save-ignore-modes
  '(c-mode c++-mode python-mode markdown-mode)
  "The modes that don't auto format and organize imports while saving the buffers.
`prog-mode' means ignoring all derived modes."
  :group 'dylan
  :type '(repeat (symbol :tag "Major-Mode")))

(defcustom dylan-chinese-calendar nil
  "Enable Chinese calendar or not."
  :group 'dylan
  :type 'boolean)

;; Load `custom-file'
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(provide 'init-custom)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-custom.el ends here
