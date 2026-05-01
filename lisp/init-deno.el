;;; init-deno.el --- JavaScript/TypeScript configuration
;;
;; JavaScript/TypeScript 开发环境配置
;; 使用 deno lsp (Rust 实现) 作为 LSP 服务器
;;
;; 安装步骤：
;; 1. 安装 deno: curl -fsSL https://deno.land/install.sh | sh
;;
;; Deno LSP 支持：
;; - JavaScript (.js)
;; - TypeScript (.ts)
;; - JSX (.jsx)
;; - TSX (.tsx)
;;

;;; Code:
(use-package js
  :mode ("\\.js\\'" . js-ts-mode))

(use-package typescript-mode
  :mode ("\\.ts\\'" . typescript-ts-mode)
  :mode ("\\.tsx\\'" . typescript-ts-mode))

;; 配置 eglot 使用 deno lsp
(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '((js-mode js-ts-mode typescript-mode typescript-ts-mode)
                 . ("deno" "lsp"))))

(provide 'init-deno)
;;; init-deno.el ends here
