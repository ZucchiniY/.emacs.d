(use-package stock-tracker
  :general
  (global-leader
    "k" 'stock-tracker-start)
  :init
  ;; 股票列表
  ;; SH 以 0 开始，SH601328 => 0601328
  ;; SZ 以 1 开始，SZ002047 => 1002047
  (setq stock-tracker-list-of-stocks '("0601328" "1002020" "1002047")
        ;; 刷新时间，s * 10 秒
        stock-tracker-refresh-interval 0.1)
  )

(provide 'modules-stock)
