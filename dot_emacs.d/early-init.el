;;; early-init.el --- eowiz's early-init.el -*- mode: emacs-lisp; lexical-binding: t; -*-

;; Copyright (C) 2023 eowiz

;;; Commentary:

;;; Code:

;; native-comp の警告を抑制
(setq native-comp-async-report-warnings-errors nil)

;; 起動時に GC が発生する頻度を下げるための設定
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.5)
(add-hook 'emacs-startup-hook
          (lambda ()
            "Recover GC values after startup."
            (setq gc-cons-threshold (* 128 1024 1024)
                  gc-cons-percentage 0.1)))

;; Mac で中華フォントが設定される問題の対策
(set-language-environment "Japanese")
(prefer-coding-system 'utf-8)

;; 自動的なパッケージのロードを無効
(setq package-enable-at-startup nil)
;; .el より .elc を常に優先
(setq load-prefer-newer t)

;; スクロールバーを無効
(push '(vertical-scroll-bars) default-frame-alist)
;; メニューバーを無効
(push '(menu-bar-lines . 0) default-frame-alist)
;; ツールバーを無効
(push '(tool-bar-lines . 0) default-frame-alist)
;; カーソルを縦棒に設定
;; (push '(cursor-type . bar) default-frame-alist)

(push '(undecorated-round . t) default-frame-alist)

(push '(left-fringe . 0) default-frame-alist)
(push '(right-fringe . 0) default-frame-alist)

;; フォント設定
(push '(font . "Cica-20") default-frame-alist)
;; (push '(font . "HackGen-20") default-frame-alist)

;; Emacs の起動画面を無効
(setq inhibit-splash-screen t)
;; フォント読み込みのタイミングなどでフレームの再計算がされるのを抑制
(setq frame-inhibit-implied-resize t)

;; タイトルバー
(push '(ns-transparent-titlebar) default-frame-alist)
(setq frame-title-format nil)

;; メッセージボックスを無効
(defalias 'message-box 'message)
(setq use-dialog-box nil)

;; ベルを無効
(setq ring-bell-function 'ignore)

;; 行番号を表示
(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(add-hook 'text-mode-hook #'display-line-numbers-mode)
;; (global-display-line-numbers-mode t)
;; ddskk を使うときのガタつきを抑制
(custom-set-variables '(display-line-numbers-width-start t))


;;; early-init.el ends here
