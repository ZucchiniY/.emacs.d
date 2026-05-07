;;; init-svelte.el --- Svelte configuration
;;
;; Svelte 开发环境配置
;; 使用 svelte-language-server 作为 LSP 服务器
;;
;; 安装步骤：
;; 1. 安装 svelte-language-server: npm install -g svelte-language-server
;;
;; Svelte LSP 支持：
;; - .svelte 文件
;; - TypeScript
;; - JavaScript
;;

;;; Code:
(use-package svelte-mode
  :mode ("\\.svelte\\'" . svelte-mode))

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '(svelte-mode . ("svelte-language-server" "--stdio"))))

(provide 'init-svelte)
;;; init-svelte.el ends here