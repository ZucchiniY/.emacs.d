;;; modules-ledger.el --- summary -*- lexical-binding: t -*-

;; Author: Dylan Yang
;; Maintainer: Dylan Yang
;; Version: 1.0.0
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

;; 序言、前言 front matter
;; 主故事 main matter
;; 后记 back matter

;;; Code:
(require 'core-org)

(use-package ledger-mode
  :ensure t
  :mode "\\.ledger\\'"
  :config
  (setq ledger-default-date-format "%Y-%m-%d"
        ledger-use-iso-dates t
        ledger-report-use-strict t
        ledger-reconcile-default-commodity "¥"))

(provide 'modules-ledger)
;;; modules-ledger.el ends here
