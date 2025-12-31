;;; init-workspace.el --- Initialize workspace configurations.	-*- lexical-binding: t -*-

;; Copyright (C) 2019-2026 Dylan Yang

;; Author: Dylan Yang <banshiliuli1990@sina.com>
;; URL: https://github.com/zucchiniy/.emacs.d

;;; Commentary:
;;
;; Workspace configurations.
;;

;;; Code:

(eval-when-compile
  (require 'init-custom))

(use-package tabspaces
  :functions tabspaces-mode
  :hook (after-init . (lambda() (unless dylan-dashboard (tabspaces-mode t))))
  :custom
  (tab-bar-show nil)

  (tabspaces-use-filtered-buffers-as-default t)
  (tabspaces-default-tab "Default")
  (tabspaces-remove-to-default t)
  (tabspaces-include-buffers '("*scratch*" "*Messages*"))
  (tabspaces-exclude-buffers '("*eat*" "*vterm*" "*shell*" "*eshell*"))
  ;; sessions
  (tabspaces-session t)
  (tabspaces-session-auto-restore (not dylan-dashboard))
  :config
  (with-no-warnings
    ;; Filter Buffers for Consult-Buffer
    (with-eval-after-load 'consult
      ;; hide full buffer list (still available with "b" prefix)
      (consult-customize consult--source-buffer :hidden t :default nil)
      ;; set consult-workspace buffer list
      (defvar consult--source-workspace
        (list :name     "Workspace Buffer"
              :narrow   ?w
              :history  'buffer-name-history
              :category 'buffer
              :state    #'consult--buffer-state
              :default  t
              :items    (lambda () (consult--buffer-query
                                    :predicate #'tabspaces--local-buffer-p
                                    :sort 'visibility
                                    :as #'buffer-name)))
        "Set workspace buffer list for consult-buffer.")
      (add-to-list 'consult-buffer-sources 'consult--source-workspace))

    (defun my-tabspaces-delete-childframe (&rest _)
      "Delete all child frames."
      (ignore-errors
        (posframe-delete-all)))
    (advice-add #'tabspaces-save-session :before #'my-tabspaces-delete-childframe)

    (defun my-tabspaces-burry-window (&rest _)
      "Burry *Messages* buffer."
      (ignore-errors
        (quit-windows-on messages-buffer-name)))
    (advice-add #'tabspaces-restore-session :after #'my-tabspaces-burry-window)))

(provide 'init-workspace)
;;; init-workspace.el ends here
