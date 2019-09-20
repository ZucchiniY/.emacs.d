;; 隐藏滚动条、菜单栏
(unless sys/winntp
  (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))
(when (fboundp 'horizontal-scroll-bar-mode)
  (horizontal-scroll-bar-mode -1))
(when sys/macp
  (setq dired-use-ls-dired nil))
(tooltip-mode nil)
(setq make-backup-files nil)

(use-package doom-themes
  :init
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t
        doom-one-brighter-comments t
        doom-one-padded-modeline t)
  :config
  (load-theme 'doom-dracula t)
  (doom-themes-visual-bell-config)
  (doom-themes-org-config)
  (doom-themes-treemacs-config))

(defun dylan//set-monospaced-font (english chinese english-size chinese-size)
  "Zty//set-monospaced-font to configuration the font.
ENGLISH is english font name
CHINESE is chinese font name
ENGLISH-SIZE is the english fond size
CHINESE-SIZE is the chinese font size."
  (set-face-attribute 'default nil
                      :font (font-spec
                             :name english
                             :weight 'normal
                             :slant 'normal
                             :size english-size))
  (dolist (charset '(kana han symbol cjk-misc bopomofo))
    (set-fontset-font (frame-parameter nil 'font)
                      charset
                      (font-spec ;;:family chinese
                       :name chinese
                       :weight 'normal
                       :slant 'normal
                       :size chinese-size))))

(if (display-graphic-p)
    (if (or sys/mac-x-p sys/linux-x-p)
        (dylan//set-monospaced-font "Iosevka Term" "黑体-简" 14 14)
      (dylan//set-monospaced-font "Iosevka Term" "Microsoft YaHei" 14 14)))

(provide 'core-ui)
