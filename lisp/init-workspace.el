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
  ;; init-custom: 用户自定义配置文件，包含个性化变量（如路径、主题等）
  ;; 在编译期加载，确保后续代码可以引用其中定义的自定义变量
  (require 'init-custom))

;; tabspaces: 基于 tab-bar 的工作区管理框架
;; 将 Emacs buffer 按命名空间（workspace/tab）分组，支持会话持久化
;; 提供每个工作区独立的 buffer 列表，实现类似 IDE 多工作区的体验
(use-package tabspaces
  :functions tabspaces-mode
  :hook (after-init . (lambda() (tabspaces-mode t)))
  :custom
  ;; 隐藏 Emacs 内置的 tab-bar UI（tabspaces 通过自己的机制管理标签）
  (tab-bar-show nil)

  ;; 默认使用过滤后的 buffer 列表（仅显示当前工作区的 buffer）
  (tabspaces-use-filtered-buffers-as-default t)
  ;; 默认工作区名称
  (tabspaces-default-tab "Default")
  ;; 关闭工作区时，将其 buffer 移回默认工作区而非直接关闭
  (tabspaces-remove-to-default t)
  ;; 以下 buffer 始终在所有工作区中可见
  (tabspaces-include-buffers '(*scratch* *Messages*))
  ;; 以下 buffer 从工作区 buffer 列表中排除（终端类 buffer）
  (tabspaces-exclude-buffers '(*eat* *vterm* *shell* *eshell*))
  ;; 启用会话持久化，保存/恢复工作区状态
  (tabspaces-session t)
  ;; 自动恢复上次保存的工作区会话
  (tabspaces-session-auto-restore t)
  ;; 不再使用单独的 :general 配置，避免与 projectile 的 SPC p 前缀冲突
  ;; tabspaces 相关功能将与 projectile 的快捷键配置整合在一起
  :config
  (with-no-warnings
    ;; consult: 增强型搜索/导航框架，提供 consult-buffer 等异步补全命令
    ;; 以下配置将 tabspaces 的工作区 buffer 列表集成到 consult-buffer 中
    (with-eval-after-load 'consult
      ;; 隐藏默认的全局 buffer 列表（仍可通过 "b" 前缀访问）
      (consult-customize consult--source-buffer :hidden t :default nil)
      ;; 定义 consult-buffer 的"工作区 buffer"数据源
      ;; 仅显示属于当前工作区的 buffer，按可见性排序
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
    ))

;; projectile: 项目管理框架，提供基于项目根目录的文件导航、搜索和切换功能
;; 自动检测项目根目录（通过 .git、.projectile 等标记文件）
;; general: 键绑定声明式定义框架，支持按 state（normal/insert/visual 等）和前缀键组织快捷键
;; 以下使用 general 的 :prefix "SPC p" 定义以 SPC p 为前缀的项目相关快捷键
(use-package projectile
  :ensure t
  :diminish
  :init
  ;; 项目根目录检测策略：本地 → 自底向上 → 自顶向下 → 自顶向下递归
  (setq projectile-project-root-files-functions
        '(projectile-root-local
          projectile-root-bottom-up
          projectile-root-top-down
          projectile-root-top-down-recording))
  ;; 用于识别项目根目录的标记文件
  (setq projectile-project-root-files '(".git" ".projectile" ".hg" ".fslckout" ".bzr" "_darcs"))
  ;; 启用项目文件缓存，加速文件查找
  (setq projectile-enable-caching t)
  ;; 使用默认补全系统（配合 consult 等框架使用）
  (setq projectile-completion-system 'default)
  :general
  (general-define-key
   :states '(normal visual)
   :keymaps 'override
   :prefix "SPC p"
   :doc "项目相关功能"
   "f" 'projectile-find-file          ;; 在当前项目中查找文件
   "g" 'consult-git-grep               ;; 在当前项目中通过 git grep 搜索内容
   "b" 'consult-project-buffer         ;; 列出当前项目的 buffer
   "s" 'projectile-switch-project      ;; 切换到另一个项目
   "t" 'treemacs                       ;; 打开 treemacs 文件树侧边栏
   "m" 'magit-status                   ;; 打开 magit Git 状态面板
   "p" 'project-find-file              ;; 使用内置 project.el 查找项目文件
   "c" 'tabspaces-open-or-create-project-and-workspace  ;; tabspaces: 打开或创建项目和工作区
   "d" 'tabspaces-kill-buffers-close-workspace)         ;; tabspaces: 关闭工作区并关闭其所有 buffer
  :config
  (projectile-mode t)
  ;; 将 projectile 的最近打开项目列表集成到 consult-buffer 中
  ;; 这样在 consult-buffer 中可以快速切换到最近访问的项目
  (with-eval-after-load 'consult
    (defvar consult-projectile-source-project
      (list :name     "Projectile Project"
            :narrow   ?p
            :history  'projectile-recentf-history
            :category 'file
            :items    #'projectile-recentf-list
            :state    #'consult--file-state)
      "Projectile projects source for `consult-buffer'.")
    (add-to-list 'consult-buffer-sources 'consult-projectile-source-project)))

;; emacs: Emacs 内置功能配置（通过 use-package 的 :after 确保在 projectile 加载后执行）
;; 以下自定义 project-switch-commands，设置切换项目时可用的操作命令及其快捷键
(use-package emacs
  :after projectile
  :config
  ;; 自定义 project-switch-commands：切换项目时弹出的命令选择列表
  ;; 每项格式为 (命令 "描述" 快捷键字符)
  (setq project-switch-commands
        '((project-find-file "Find file" ?f)       ;; 在项目中查找文件
          (project-grep "Grep" ?g)                 ;; 在项目中搜索文本
          (project-find-regexp "Find regexp" ?r)   ;; 在项目中搜索正则表达式
          (project-dired "Dired" ?d)               ;; 以 Dired 模式打开项目目录
          (project-eshell "Eshell" ?e)             ;; 在项目目录中打开 Eshell
          (project-vc-dir "VC" ?v)                 ;; 打开项目版本控制目录
          (project-compile "Compile" ?c))))        ;; 执行项目编译命令

(provide 'init-workspace)
;;; init-workspace.el ends here
