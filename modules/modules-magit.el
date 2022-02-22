;;; modules-magit.el --- summary -*- lexical-binding: t -*-

;; Author: Dylan Yang
;; Maintainer: Dylan Yang
;; Version: 1.0.1
;; Package-Requires: ( )
;; Homepage: 
;; Keywords: 


;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.


;;; Commentary:

;; commentary

;;; Code:

(use-package transient
  :ensure t
  :commands (transient-setup transient-prefix))

(use-package magit
  :general
  (global-leader
    "g g" 'magit-status
    "g ." 'magit-dispatch)
  :config
  (when sys/winntp
    (setenv "GIT_ASKPASS" "git-gui--askpass"))

  ;; Access Git forges from Magit
  ;; https://github.com/skeeto/emacsql#windows-issues
  (unless sys/winntp
    (use-package forge :demand))

  ;; Show tasks
  (use-package magit-todos
    :hook (magit-status-mode . magit-todos-mode)
    :init (magit-todos-mode 1)))

;; Walk through git revisions of a file
(use-package git-timemachine
  :bind (:map vc-prefix-map
              ("t" . git-timemachine)))

;; ;; Highlighting regions by last updated time
;; (use-package smeargle
;;   :bind (:map vc-prefix-map
;;               ("S" . smeargle)
;;               ("C" . smeargle-commits)
;;               ("R" . smeargle-clear)))

;; Git related modes
;; (use-package gitattributes-mode)
;; (use-package gitconfig-mode)
;; (use-package gitignore-mode)

(provide 'modules-magit)

;;; modules-magit.el ends here
