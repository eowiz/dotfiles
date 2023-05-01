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

  (global-auto-revert-mode 1)

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
    (load-theme 'modus-operandi-tinted t)))

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
	 :map minibuffer-mode-map
	 ("M-s" . consult-history)
         ("M-r" . consult-history))
  :custom
  (consult-preview-raw-size 1024000)
  (consult-preview-max-size 1024000)
  :config
  (consult-customize
   consult-buffer :preview-key "M-.")

  (defhydra hydra-consult (:hint nil :exit t)
    "
  Serach
  ^^^^^^^^^^─────────────────────────────────────────────────────────────────╨─────────╜
  [_l_] line
  [_L_] line-multi
  [_g_] git-grep
  [_r_] ripgrep
  "
    ("l" consult-line)
    ("L" consult-line-multi)
    ("g" consult-git-grep)
    ("r" consult-ripgrep)))

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

;; elscreen
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
  :defer 1)

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

(use-package flycheck
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
  (setq sideline-backends-skip-current-line t  ; don't display on current line
        sideline-order-left 'down              ; or 'up
        sideline-order-right 'up               ; or 'down
        sideline-format-left "%s   "           ; format for left aligment
        sideline-format-right "   %s"          ; format for right aligment
        sideline-priority 100                  ; overlays' priority
        sideline-display-backend-name t)
  (setq sideline-backends-right '(sideline-flycheck)))

(use-package sideline-flycheck
  :disabled t
  :straight t
  :defer t
  :hook ((flycheck-mode . sideline-flycheck-setup)))

(use-package rainbow-delimiters
  :straight t
  :defer t
  :hook ((prog-mode . rainbow-delimiters-mode)))

(use-package vterm
  :straight t
  :defer t)

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

(use-package org-mode
  :straight t
  :defer t
  :custom
  (org-ellipsis " ▼"))

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
  :init
  (setq org-modern-star (list #("󰎥" 0 1 (face (:family "Symbols Nerd Font Mono" :height 1.0) font-lock-face (:family "Symbols Nerd Font Mono" :height 1.0) display (raise 0.0) rear-nonsticky t))
			      #("󰎨" 0 1 (face (:family "Symbols Nerd Font Mono" :height 1.0) font-lock-face (:family "Symbols Nerd Font Mono" :height 1.0) display (raise 0.0) rear-nonsticky t))
			      #("󰎫" 0 1 (face (:family "Symbols Nerd Font Mono" :height 1.0) font-lock-face (:family "Symbols Nerd Font Mono" :height 1.0) display (raise 0.0) rear-nonsticky t))
			      #("󰎲" 0 1 (face (:family "Symbols Nerd Font Mono" :height 1.0) font-lock-face (:family "Symbols Nerd Font Mono" :height 1.0) display (raise 0.0) rear-nonsticky t))
			      #("󰎯" 0 1 (face (:family "Symbols Nerd Font Mono" :height 1.0) font-lock-face (:family "Symbols Nerd Font Mono" :height 1.0) display (raise 0.0) rear-nonsticky t))
			      ))
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
  :straight (lsp-bridge :host github
			:repo "manateelazycat/lsp-bridge"
			:files ("*.el" "*.py" "acm" "core" "languageserver"
				"multiserver" "resources"))
  :defer 2
  :hook ((acm-mode . (lambda () (corfu-mode -1)))))

(use-package acm-terminal
  :straight (acm-terminal :host github :repo "twlz0ne/acm-terminal")
  :defer t)

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

