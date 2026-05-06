;;; init-dart.el --- Dart configuration
;;
;; Dart 开发环境配置
;; 使用 dart analysis-server 作为 LSP 服务器
;;
;; 安装步骤：
;; 1. macOS: brew install dart
;;    Linux: sudo apt install dart 或从官网下载
;; 2. Flutter 开发: flutter 会自带 Dart SDK
;;

;;; Code:
(use-package dart-mode
  :mode ("\\.dart\\'" . dart-mode)
  :init
  (setq dart-enable-analysis-server t
        dart-format-on-save t))

(provide 'init-dart)
;;; init-dart.el ends here
