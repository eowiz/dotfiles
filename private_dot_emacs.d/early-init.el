(setq package-enable-at-startup nil)
(setq load-prefer-newer t)

(setq inhibit-splash-screen t)

 ;; スクロールバーを無効
(push '(vertical-scroll-bars) default-frame-alist)
;; メニューバーを無効
(push '(menu-bar-lines . 0) default-frame-alist)
;; ツールバーを無効
(push '(tool-bar-lines . 0) default-frame-alist)

