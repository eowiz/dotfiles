;; straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

;;;###autoload
(defun open-init-org ()
  "Toggle current buffer between init.el."
  (interactive)
  (find-file (locate-user-emacs-file "init.el")))

(use-package emacs
  :bind (("M-SPC" . open-init-org))
  :init
  (setq scroll-margin 0
	scroll-conservatively 100000
	scroll-preserve-screen-position t)
  (pixel-scroll-precision-mode t)

  (setq dired-do-revert-buffer t)
  (setq global-auto-revert-non-file-buffers t)
  (global-auto-revert-mode)

  (savehist-mode)
  (recentf-mode)

  (global-hl-line-mode)

  (setq indent-tabs-mode nil
	make-backup-files nil
	auto-save-default nil

	delete-by-moving-to-trash t)
  (setq insert-directory-program "gls")
  (defalias 'yes-or-no-p 'y-or-n-p))

(use-package exec-path-from-shell
  :straight t
  :config
  (exec-path-from-shell-initialize))

(use-package modus-themes
  :straight t
  :defer t
  :hook (emacs-startup . my/load-theme)
  :init
  (defun my/load-theme ()
    (load-theme 'modus-operandi-tinted t)
    ;; (load-theme 'modus-vivendi t)
    ;; (load-theme 'modus-vivendi-tinted t)
    ;; (load-theme 'modus-vivendi-deuteranopia t)
    ))

(use-package doom-modeline
  :straight t
  :config
  (doom-modeline-mode 1))

(use-package hydra
  :straight t
  :defer t)

;; vertico
(use-package vertico
  :straight t
  :defer t
  :init
  (vertico-mode))

(use-package consult
  :straight t
  :defer t
  :after (hydra)
  :bind (("C-c M-x" . consult-mode-command)
	 ("C-c k" . consult-kmacro)
	 ("C-x b" . consult-buffer)
	 :map isearch-mode-map
	 ("M-l" . consult-line)
	 ("M-g" . consult-ripgrep)
	 :map minibuffer-local-map
	 ("M-s" . consult-history)
         ("M-r" . consult-history))
  :custom
  (consult-preview-raw-size 1024000)
  (consult-preview-max-size 1024000)
  (xref-show-xrefs-function #'consult-xref)
  (xref-show-definitions-function #'consult-xref)
  :config
  (consult-customize
   consult-buffer :preview-key "M-.")

  (defhydra hydra-consult (:hint nil :exit t)
    "
   ^ ^               ^ ^                                                ╔═════════╗
  Serach^^          Jump^^                                              ║ Consult ║
  ^^^^──────────────────────────────────────────────────────────────────╨─────────╜
  [_l_] line        [_m_] imenu
  [_L_] line-multi  [_M_] imenu-multi
  [_g_] git-grep    [_o_] outline
  [_r_] ripgrep
  ╭^^^^^───────────────────────────────────────────────────────────────────────────╯
                                  [_q_]: quit
  "
    ("l" consult-line)
    ("L" consult-line-multi)
    ("g" consult-git-grep)
    ("r" consult-ripgrep)
    ("m" consult-imenu)
    ("M" consult-imenu-multi)
    ("o" consult-outline)
    ("q" nil nil)))

(use-package orderless
  :straight t
  :defer t
  :custom
  (completion-styles '(orderless basic)))

(use-package marginalia
  :straight t
  :defer 1
  :commands (marginalia-mode)
  :hook ((emacs-startup . marginalia-mode)))

(use-package consult-ghq
  :straight t
  :defer t
  :commands (consult-ghq-find)
  :bind (("C-c C-g" . consult-ghq-find)))

;; ddskk
(use-package ddskk
  :straight t
  :defer t
  :commands (skk-mode skk-latin-mode)
  :init
  (defun my/enable-skk-latin-mode ()
    (skk-mode 1)
    (skk-latin-mode 1))
  :bind (("C-x C-j" . skk-mode))
  :hook ((after-change-major-mode . my/enable-skk-latin-mode)))

(use-package ddskk-posframe
  :straight t
  :defer t
  :commands (ddskk-posframe-mode)
  :hook ((skk-mode . ddskk-posframe-mode)))

(use-package tab-bar
  :straight t
  :demand t
  :after (hydra nerd-icons)
  :init
  (define-prefix-command 'my/tab-bar-map)

  (defhydra hydra-tab-bar (:color blue :hint nil :exit nil)
    "
             ^^^^             ^^           ^^^^                              ╔═════════╗
   Swich/Move^^^^      Actions^^       Move^^^^                              ║ Tab Bar ║
  ^^^^^^^^^^─────────────────────────────────────────────────────────────────╨─────────╜
   _n_ ← switch → _p_  [_c_] new tab   [_C-1_] move to 1  [_C-6_] move to 6
   ^ ^            ^ ^  [_d_] duplicate [_C-2_] move to 2  [_C-7_] move to 7
   _b_ ←  move  → _f_  ^  ^            [_C-3_] move to 3  [_C-8_] move to 8
   ^ ^            ^ ^  ^  ^            [_C-4_] move to 4  [_C-9_] move to 9
   ^ ^            ^ ^  ^  ^            [_C-5_] move to 5  [_C-0_] move to 10
  ╭^^^^^^^^^^───────────────────────────────────────────────────────────────────────────╯
                                    [_q_]: quit
"
    ("n" tab-bar-switch-to-next-tab nil :exit nil)
    ("p" tab-bar-switch-to-prev-tab nil :exit nil)
    ("c" tab-bar-new-tab nil :exit nil)
    ("d" tab-bar-duplicate-tab nil :exit nil)
    ("b" tab-bar-move-tab-backward nil :exit nil)
    ("f" tab-bar-move-tab nil :exit nil)
    ("C-1" (tab-bar-move-tab-to 1) nil :exit nil)
    ("C-2" (tab-bar-move-tab-to 2) nil :exit nil)
    ("C-3" (tab-bar-move-tab-to 3) nil :exit nil)
    ("C-4" (tab-bar-move-tab-to 4) nil :exit nil)
    ("C-5" (tab-bar-move-tab-to 5) nil :exit nil)
    ("C-6" (tab-bar-move-tab-to 6) nil :exit nil)
    ("C-7" (tab-bar-move-tab-to 7) nil :exit nil)
    ("C-8" (tab-bar-move-tab-to 8) nil :exit nil)
    ("C-9" (tab-bar-move-tab-to 9) nil :exit nil)
    ("C-0" (tab-bar-move-tab-to 0) nil :exit nil)
    ("q" nil nil))
  :custom
  (tab-bar-tab-hints t)
  :bind-keymap
  ("C-z" . my/tab-bar-map)
  :bind (:map my/tab-bar-map
	      ("h" . hydra-tab-bar/body)
	      ("n" . tab-bar-switch-to-next-tab)
	      ("p" . tab-bar-switch-to-prev-tab)
	      ("r" . tab-bar-switch-to-recent-tab)
	      ("c" . tab-bar-new-tab)
	      ("x" . tab-bar-close-tab)
	      ("1" . tab-select)
	      ("2" . tab-select)
	      ("3" . tab-select)
	      ("4" . tab-select)
	      ("5" . tab-select)
	      ("6" . tab-select)
	      ("7" . tab-select)
	      ("8" . tab-select)
	      ("9" . tab-select))
  :config
  ;; see: https://christiantietze.de/posts/2022/02/emacs-tab-bar-numbered-tabs/
  ;;    : https://christiantietze.de/posts/2022/12/sf-symbols-emacs-tab-numbers/
  (defface ct/tab-bar-numbers
    '((t
       :inherit tab-bar
       :family "Symbols Nerd Font Mono"
       :weight light))
    "Face for tab numbers in both active and inactive tabs.")
  (defvar ct/box-numbers-alist
    `((1 . "󰲡")
      (2 . "󰲣")
      (3 . "󰲥")
      (4 . "󰲧")
      (5 . "󰲩")
      (6 . "󰲫")
      (7 . "󰲭")
      (8 . "󰲯")
      (9 . "󰲱")
      (0 . "󰿭"))
    "Alist of integers to strings of SF Symbols with numbers in boxes.")

  (defun ct/tab-bar-tab-name-format-default (tab i)
    (let ((current-p (eq (car tab) 'current-tab)))
      (concat
       ;; First, add the tab number with a custom face
       (propertize
	(when (and tab-bar-tab-hints (< i 10)) (alist-get i ct/box-numbers-alist))
	'face 'ct/tab-bar-numbers)
       ;; Add a space (unstyled)
       " "
       ;; Add tab name with the face returned by tab-bar-tab-face-function
       (propertize
	(concat (alist-get 'name tab)
	        (or (and tab-bar-close-button-show
			 (not (eq tab-bar-close-button-show
				  (if current-p 'non-selected 'selected)))
			 tab-bar-close-button)
		    ""))
	'face (funcall tab-bar-tab-face-function tab))
       ;; Add trailing space (unstyled)
       "")))
  (setq tab-bar-tab-name-format-function #'ct/tab-bar-tab-name-format-default)

  (defun ct/modus-themes-tab-bar-colors ()
    "Override `modus-themes-tab-*' to have even less variety"
    (let* ((bg-color (modus-themes-color 'bg-main))
           ;; Additional padding between tabs
           (box `(:line-width
                  (2 . -1) ;; -1 for no vertical space
                  :color ,bg-color :style flat-button))
           (active-accent-color (modus-themes-color 'blue-active)))
      (set-face-attribute 'tab-bar nil
                          :height 0.8)
      (set-face-attribute 'modus-themes-tab-backdrop nil
                          :background bg-color
                          :box nil)
      (set-face-attribute 'modus-themes-tab-inactive nil
                          :background bg-color
                          :box box)
      (set-face-attribute 'modus-themes-tab-active nil
                          :background bg-color
                          :underline `(:color ,active-accent-color :style line)
                          :box box)))
  (add-hook 'modus-themes-after-load-theme-hook #'ct/modus-themes-tab-bar-colors)

  (tab-bar-mode 1))

(use-package mlscroll
  :straight t
  :defer t
  :hook ((emacs-startup . mlscroll-mode)))

(use-package ace-window
  :straight t
  :defer t
  :bind (("C-x o" . ace-window))
  :custom
  (aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  :config
  (custom-set-faces
   '(aw-leading-char-face
     ((t
       (:foreground "deep sky blue" :bold t :height 3.0))))))

(use-package avy
  :straight t
  :defer t)

(use-package migemo
  :straight t
  :defer t
  :custom
  (migemo-options '("-q" "--emacs"))
  (migemo-command "/opt/homebrew/bin/cmigemo")
  (migemo-dictionary "/opt/homebrew/share/migemo/utf-8/migemo-dict")
  (migemo-user-dictionary nil)
  (migemo-regex-dictionary nil)
  (migemo-coding-system 'utf-8-unix)
  :config
  (migemo-init))

(use-package avy-migemo
  :straight t
  :defer t
  :bind (("C-;" . avy-migemo-goto-char-timer))
  :custom
  (avy-timeout-seconds nil)
  :config
  (avy-migemo-mode 1))

(use-package anzu
  :straight t
  :defer 1
  :commands (anzu-query-replace anzu-query-replace-regex anzu-mode-line)
  :bind (([remap query-replace] . anzu-query-replace)
	 ([remap query-replace-regexp] . anzu-query-replace-regex))
  :custom ((anzu-replace-threshold 1000)
	   (anzu-search-threshold 1000))
  :hook ((emacs-startup . (lambda () (global-anzu-mode +1))))
  :config
  (copy-face 'mode-line 'anzu-mode-line))

(use-package nerd-icons
  :straight t
  :defer 1
  :config
  (delete '("code" nerd-icons-octicon "nf-oct-code") nerd-icons-dir-icon-alist))

(use-package nerd-icons-dired
  :straight t
  :defer 1
  :commands (nerd-icons-dired-mode)
  :hook ((dired-mode . nerd-icons-dired-mode)))

(use-package corfu
  :straight t
  :defer 1
  :custom ((corfu-auto t))
  :config
  (global-corfu-mode))

(use-package kind-icon
  :straight t
  :defer t
  :custom ((kind-icon-default-face 'corfu-default))
  :hook ((corfu-mode . (lambda () (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter)))))

(use-package which-key
  :straight t
  :defer 2
  :config
  (which-key-mode)
  (which-key-setup-side-window-bottom))

(use-package volatile-highlights
  :straight t
  :defer 2
  :commands (volatile-highlgihts-mode)
  :custom-face
  (vhl/default-face ((nil (:foreground "#FF3333" :background "#FFCDCD"))))
  :config
  (volatile-highlights-mode))

(use-package highlight-indent-guides
  :straight t
  :defer 2
  :init
  (defun my/highlight-indent-guides--bitmap-line (width height crep zrep)
    "Defines a solid guide line, one pixel wide.
Use WIDTH, HEIGHT, CREP, and ZREP as described in
`highlight-indent-guides-bitmap-function'."
    (let* ((left (/ (- width 1) 2))
           (right (- width left 1))
           (row (append (make-list left zrep) (make-list 1 crep) (make-list right zrep)))
           rows)
      (dotimes (i height rows)
	(setq rows (cons row rows)))))
  :custom ((highlight-indent-guides-method 'bitmap)
	   (highlight-indent-guides-bitmap-function #'my/highlight-indent-guides--bitmap-line)
	   (highlight-indent-guides-auto-character-face-perc 40)
	   (highlight-indent-guides-auto-top-character-face-perc 100)
	   (highlight-indent-guides-auto-enabled t)
	   (highlight-indent-guides-responsive t))
  :hook ((prog-mode . highlight-indent-guides-mode)))

(use-package dimmer
  :straight t
  :defer t
  :hook ((emacs-startup . dimmer-mode))
  :custom
  (dimmer-fraction 0.6)
  :config
  (dimmer-configure-which-key))

;;
;; Git
;;

(use-package blamer
  :straight t
  :defer t)

(use-package git-timemachine
  :straight t
  :defer t)

(use-package git-modes
  :straight t
  :defer t)

(use-package git-gutter
  :straight t
  :defer 2
  :custom ((git-gutter:unchaged-sign " ")
	   (git-gutter:modified-sign " ")
	   (git-gutter:added-sign " ")
	   (git-gutter:deleted-sign " "))
  :config
  (set-face-background 'git-gutter:unchanged (face-attribute 'line-number :background))
  (set-face-background 'git-gutter:modified "#f1fa8c")
  (set-face-background 'git-gutter:added "#50fa7b")
  (set-face-background 'git-gutter:deleted "#ff79c6")

  (global-git-gutter-mode +1))

(use-package magit
  :straight t
  :defer t
  :commands (magit)
  :bind (("C-x g" . magit)))

;;
;; undo
;;

(use-package undohist
  :straight t
  :defer 2
  :config
  (undohist-initialize))

(use-package undo-tree
  :straight t
  :defer 2
  :custom
  (undo-tree-auto-save-history nil)
  :config
  (global-undo-tree-mode))

(use-package flymake
  :straight t
  :defer t)

(use-package flymake-diagnostic-at-point
  :straight t
  :defer t)

(use-package posframe
  :straight t
  :defer t)

(use-package flymake-posframe
  :straight (flymake-posframe :type git :host github :repo "Ladicle/flymake-posframe")
  :defer t
  :hook (flymake-mode . flymake-posframe-mode))

(use-package flycheck
  :disabled t
  :straight t
  :defer 2
  :config
  (global-flycheck-mode))

(use-package sideline
  :disabled t
  :straight t
  :defer t
  :hook ((flycheck-mode . sideline-mode))
  :init
  (setq sideline-backends-skip-current-line t ; don't display on current line
        sideline-order-left 'down	      ; or 'up
        sideline-order-right 'up	      ; or 'down
        sideline-format-left "%s   "	; format for left aligment
        sideline-format-right "   %s"	; format for right aligment
        sideline-priority 100		; overlays' priority
        sideline-display-backend-name t)
  (setq sideline-backends-right '(sideline-flycheck))
  :config
  (use-package sideline-flycheck
    :straight t
    :defer t
    :hook ((flycheck-mode . sideline-flycheck-setup))))

(use-package rainbow-delimiters
  :straight t
  :defer t
  :hook ((prog-mode . rainbow-delimiters-mode)))

(use-package vterm
  :straight t
  :defer t)

(use-package vterm-toggle
  :straight t
  :defer t
  :bind (("C-'" . vterm-toggle))
  :custom
  (vterm-toggle-scope 'project)
  :config
  (setq vterm-toggle-fullscreen-p nil)
  (add-to-list 'display-buffer-alist
               '((lambda (buffer-or-name _)
                   (let ((buffer (get-buffer buffer-or-name)))
                     (with-current-buffer buffer
                       (or (equal major-mode 'vterm-mode)
                           (string-prefix-p vterm-buffer-name (buffer-name buffer))))))
		 (display-buffer-reuse-window display-buffer-in-side-window)
		 (side . bottom)
		 ;;(dedicated . t) ;dedicated is supported in emacs27
		 (reusable-frames . visible)
		 (window-height . 0.3))))

(use-package projectile
  :straight t
  :defer t
  :bind (:map projectile-mode-map
	      ("s-p" . projectile-command-map))
  :hook ((emacs-startup . projectile-mode))
  :custom
  (projectile-generic-command "fd . -0 --type f --color=never"))

(use-package projectile-ripgrep
  :straight t
  :defer t)

(use-package consult-projectile
  :straight (consult-projectile :type git :host gitlab :repo "OlMon/consult-projectile" :branch "master")
  :defer t)

(use-package treemacs
  :straight t
  :defer t
  :bind (("C-\\" . treemacs-select-window))
  :config
  (treemacs-indent-guide-mode t)
  (treemacs-fringe-indicator-mode t)
  (treemacs-filewatch-mode t)
  (treemacs-git-commit-diff-mode t)

  (pcase (cons (not (null (executable-find "git")))
               (not (null (treemacs--find-python3))))
    (`(t . t)
     (treemacs-git-mode 'deferred))
    (`(t . _)
     (treemacs-git-mode 'simple)))
  
  (use-package treemacs-nerd-icons
    :straight t
    :config
    (treemacs-load-theme "nerd-icons")))

(use-package treemacs-projectile
  :straight t
  :defer t)

(use-package treemacs-magit
  :straight t
  :defer t)

(use-package apheleia
  :straight t
  :defer t
  :config
  (apheleia-global-mode +1))

(use-package org-mode
  :straight t
  :defer t
  :init
  (setq system-time-locate nil)
  :custom
  (org-ellipsis " ▼")
  (org-fontify-quote-and-verse-blocks t)
  (org-use-speed-commands t)

  (org-display-custom-times t)
  (org-image-actual-width nil)

  ;; see: https://misohena.jp/blog/2021-08-29-colorize-saturday-and-japanese-holidays-in-org-agenda.html
  (org-agenda-day-face-function (lambda (date)
				  (let ((face (cond
					       ;; 土曜日
					       ((= (calendar-day-of-week date) 6)
						'(:inherit org-agenda-date :foreground "#0df"))
					       ;; 日曜日か日本の祝日
					       ((or (= (calendar-day-of-week date) 0)
						    (let ((calendar-holidays japanese-holidays))
						      (calendar-check-holidays date)))
						'org-agenda-date-weekend)
					       ;; 普通の日
					       (t 'org-agenda-date))))
				    ;; 今日は色を反転
				    (if (org-agenda-today-p date) (list :inherit face :inverse-video t) face))))
  :config
  (setq org-time-stamp-custom-formats '("<%Y年%m月%d日(%a)>" . "<%Y年%m月%d日(%a)%H時%M分>")))

(use-package org-bars
  :straight (org-bars :type git
		      :host github
		      :repo "tonyaldon/org-bars")
  :defer t
  :hook ((org-mode . (lambda ()
		       (require 'org-bars)
		       (org-bars-mode)))))

(use-package org-modern
  :straight t
  :defer t
  :hook ((emacs-startup . global-org-modern-mode))
  :custom
  (org-modern-hide-stars nil)
  (org-modern-list
   '((?- . "-")
     (?* . "•")
     (?+ . "‣")))
  (org-modern-timestamp '(" %Y年%m月%d日(%a) " . " %H時%M分 "))
  :init
  (setq org-modern-star (list #("󰎥" 0 1 (face (:family "Symbols Nerd Font Mono" :height 1.0) font-lock-face (:family "Symbols Nerd Font Mono" :height 1.0) display (raise 0.0) rear-nonsticky t))
			      #("󰎨" 0 1 (face (:family "Symbols Nerd Font Mono" :height 1.0) font-lock-face (:family "Symbols Nerd Font Mono" :height 1.0) display (raise 0.0) rear-nonsticky t))
			      #("󰎫" 0 1 (face (:family "Symbols Nerd Font Mono" :height 1.0) font-lock-face (:family "Symbols Nerd Font Mono" :height 1.0) display (raise 0.0) rear-nonsticky t))
			      #("󰎲" 0 1 (face (:family "Symbols Nerd Font Mono" :height 1.0) font-lock-face (:family "Symbols Nerd Font Mono" :height 1.0) display (raise 0.0) rear-nonsticky t))
			      #("󰎯" 0 1 (face (:family "Symbols Nerd Font Mono" :height 1.0) font-lock-face (:family "Symbols Nerd Font Mono" :height 1.0) display (raise 0.0) rear-nonsticky t))
			      ))
  :config
  (let ((comment-color (face-attribute 'font-lock-comment-face :foreground)))
    (custom-theme-set-faces
     'user
     `(org-quote ((t (:inherit org-block :slant italic :foreground ,comment-color))))))
  )

(use-package org-modern-indent
  :disabled t
  :straight (org-modern-indent :type git
			       :host github
			       :repo "jdtsmith/org-modern-indent")
  :defer t
  :config
  (add-hook 'org-mode-hook #'org-modern-indent-mode 90))

(use-package org-tree-slide
  :straight t
  :defer t
  :config
  (org-tree-slide-simple-profile))

(use-package org-download
  :straight t
  :defer t
  :hook ((dired-mode . org-download-enable)
	 (org-mode . org-download-enable))
  :init
  (setq org-download-image-dir "images")
  :custom
  (org-download-method 'directory))

(use-package markdown-mode
  :straight t
  :defer t)

;;
;; Programming
;;

(use-package yasnippet
  :straight t
  :defer 2
  :config
  (yas-global-mode 1))

(use-package paredit
  :straight t
  :defer t
  :commands (enable-paredit-mode)
  :hook ((emacs-lisp-mode . enable-paredit-mode)))

(use-package lsp-bridge
  :disabled t
  :straight (lsp-bridge :host github
			:repo "manateelazycat/lsp-bridge"
			:files ("*.el" "*.py" "acm" "core" "languageserver"
				"multiserver" "resources"))
  :defer 2
  :hook ((acm-mode . (lambda () (corfu-mode -1))))
  :config

  (use-package acm-terminal
  :straight (acm-terminal :host github :repo "twlz0ne/acm-terminal")
  :defer t))

(defvar lombok-path (expand-file-name "~/.m2/repository/org/projectlombok/lombok/1.18.26/lombok-1.18.26.jar"))

(use-package eglot
  :disabled t
  :straight t
  :defer t
  :config
  (use-package eglot-java
    :straight t
    :defer t
    :hook ((java-ts-mode . eglot-java-mode))
    :custom
    (eglot-java-eclipse-jdt-args
     `(
       "-noverify"
       "-Xmx1G"
       "-XX:+UseG1GC"
       "-XX:+UseStringDeduplication"
       ,(concat "-javaagent:" lombok-path)
       ,(concat "-Xbootclasspath/a:" lombok-path)))))

(use-package lsp-mode
  :straight t
  :defer t
  :hook ((lsp-mode . lsp-enable-which-key-integration)))

(use-package consult-lsp
  :straight t
  :defer t
  :after (lsp-mode consult)
  :bind (:map lsp-mode-map
	      ([remap xref-find-apropos] . consult-lsp-symbols)))

(use-package lsp-treemacs
  :straight t
  :defer t)

(use-package lsp-java
  :straight t
  :defer t
  :after (lsp-mode)
  :custom
  (lsp-java-vmargs
   `("-noverify"
     "-Xmx4G"
     "-XX:+UseG1GC"
     "-XX:+UseStringDeduplication"
     ,(concat "-javaagent:" lombok-path)
     ,(concat "-Xbootclasspath/a:" lombok-path)))

  (add-hook 'java-mode-hook #'lsp))

(use-package lsp-ui
  :straight t
  :defer t
  :after (lsp-mode)
  :hook ((lsp-mode . lsp-ui-mode))
  :bind (:map lsp-ui-mode-map
	      ("C-," . lsp-ui-doc-show)
	      ("C-." . lsp-ui-doc-focus-frame)
	      ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
	      ([remap xref-find-references] . lsp-ui-peek-find-references))
  :custom
  (lsp-print-io nil)
  (lsp-trace nil)
  (lsp-print-performance nil)

  (lsp-ui-doc-use-childframe t)
  ;; (lsp-ui-doc-use-webkit t)

  (lsp-ui-flycheck-enable nil)

  (lsp-auto-guess-root t)
  (lsp-ui-sideline-show-hover t)
  (lsp-ui-sideline-show-diagnostics t)
  (lsp-ui-peek-enable t)
  (lsp-ui-doc-enable t)
  (lsp-ui-doc-position 'at-point)
  (lsp-document-sync-method 'incremental))

(use-package tree-sitter
  :disabled t
  :straight t
  :defer t
  :hook ((tree-sitter-after-on-hook . tree-sitter-hl-mode))
  :config
  (global-tree-sitter-mode))

(use-package tree-sitter-langs
  :disabled t
  :straight t
  :defer t
  :after tree-sitter)

(use-package java-mode
  :hook ((java-mode . flymake-mode)))

(use-package yaml-mode
  :straight t
  :defer t)

(use-package groovy-mode
  :straight t
  :defer t)

(use-package rustic
  :straight t
  :defer t
  :init
  (defun rustic-mode-auto-save-hook ()
    "Enable auto-saving in rustic-mode buffers."
    (when buffer-file-name
      (setq-local compilation-ask-about-save nil)))
  :hook ((rustic-mode . rustic-mode-auto-save-hook)))

(use-package nim-mode
  :straight t
  :defer t)

(use-package elm-mode
  :straight t
  :defer t)

;; application-framework

(use-package eaf
  :disabled t
  :straight (eaf :host github
		 :repo "emacs-eaf/emacs-application-framework"
		 :files ("*.el" "*.py" "*.json"
			 "core" "swaymsg-treefetch" "extension" "app")
		 :includes (eaf-pdf-viewer eaf-browser eaf-git eaf-terminal)
		 :pre-build ("python3" "install-eaf.py" "--install" "pdf-viewer" "browser" "git" "terminal" "--ignore-sys-deps"))
  :defer 1
  :config

  (use-package eaf-browser
    :defer 3
    :custom
    (browse-url-browser-function #'eaf-open-browser)
    (eaf-browser-enable-adblocker t)
    :config
    (defalias 'browse-web #'eaf-open-browser))

  (use-package eaf-terminal
    :defer 3
    :custom
    (eaf-terminal-font-family "HackGen Console NFJ")
    (eaf-terminal-font-size 20))

  (use-package eaf-git
    :defer 3)

  (use-package eaf-pdf-viewer
    :defer 3))

;; other

(use-package elfeed
  :straight t
  :defer t
  :custom
  (elfeed-search-title-max-width 100)
  :config
  (use-package elfeed-org
    :straight t
    :custom    
    (rmh-elfeed-org-file `(,(locate-user-emacs-file "elfeed.org")))
    :config
    (elfeed-org))

  (defun elfeed-search-format-date (date)
    (format-time-string "%Y-%m-%d %H:%M" (seconds-to-time date)))

  (setq elfeed-show-mode-hook
	(lambda ()
	  (set-face-attribute 'variable-pitch (selected-frame)
			      :font (font-spec :family "HackGen Console NFJ" :size 14))
	  (setq fill-column 120)
	  (setq elfeed-show-entry-switch #'my-show-elfeed)))

  (defun my-show-elfeed (buffer)
    (with-current-buffer buffer
      (setq buffer-read-only nil)
      (goto-char (point-min))
      (re-search-forward "\n\n")
      (fill-individual-paragraphs (point) (point-max))
      (setq buffer-read-only t))
    (switch-to-buffer buffer)))

(use-package md4rd
  :straight t
  :defer t)

;; evil

(use-package evil
  :straight t
  :demand t
  :hook ((emacs-startup . evil-mode)
	 (evil-mode . my/evil-setup))
  :custom
  (evil-want-integration t)
  (evil-want-keybinding nil)
  (evil-toggle-key "C-t")
  :init
  :config
  (defun my/evil-setup ()
    (setq evil-insert-state-map nil)
    (bind-keys :map evil-insert-state-map
	       ([escape] . evil-normal-state)))

  (use-package evil-collection
    :straight t
    :config
    (evil-collection-init))

  (use-package evil-paredit
    :straight t
    :hook ((emacs-lisp-mode . evil-paredit-mode)))

  ;; TODO 動くようにする
  (use-package evil-org
    :disabled t
    :straight t
    :hook ((org-mode . (lambda () (evil-org-mode))))
    :config
    (require 'evil-org-agenda)
    (evil-org-agenda-set-keys))

  ;; see: https://tarao.hatenablog.com/entry/20130304/evil_config
  (defadvice update-buffer-local-cursor-color
      (around evil-update-buffer-local-cursor-color-in-insert-state activate)
    ;; SKKによるカーソル色変更を, 挿入ステートかつ日本語モードの場合に限定
    "Allow ccc to update cursor color only when we are in insert
state and in `skk-j-mode'."
    (when (and (eq evil-state 'insert) (bound-and-true-p skk-j-mode))
      ad-do-it))
  (defadvice evil-refresh-cursor
      (around evil-refresh-cursor-unless-skk-mode activate)
    ;; Evilによるカーソルの変更を, 挿入ステートかつ日本語モードではない場合に限定
    "Allow ccc to update cursor color only when we are in insert
state and in `skk-j-mode'."
    (unless (and (eq evil-state 'insert) (bound-and-true-p skk-j-mode))
      ad-do-it))
  (defadvice evil-ex-search-update-pattern
      (around evil-inhibit-ex-search-update-pattern-in-skk-henkan activate)
    ;; SKKの未確定状態(skk-henkan-mode)ではない場合だけ, 検索パターンをアップデート
    "Inhibit search pattern update during `skk-henkan-mode'.
This is reasonable since inserted text during `skk-henkan-mode'
is a kind of temporary one which is not confirmed yet."
    (unless (bound-and-true-p skk-henkan-mode)
      ad-do-it)))

(use-package origami
  :straight t
  :defer t
  :hook ((emacs-startup . global-origami-mode)))
