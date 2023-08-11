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

(use-package emacs
  :init
  (add-hook 'prog-mode-hook #'display-line-numbers-mode)
  (add-hook 'text-mode-hook #'display-line-numbers-mode)

  (setq native-comp-async-report-warnings-errors nil)

  (setq custom-file "~/.emacs.d/custom.el")
  (load custom-file)

  (setq scroll-margin 0
	scroll-conservatively 100000
	scroll-preserve-screen-position t)
  (setq scroll-preserve-screen-position t)

  (setq global-auto-revert-non-file-buffers t)
  (global-auto-revert-mode)

  (savehist-mode)
  (recentf-mode)

  (global-hl-line-mode)

  (setq indent-tabs-mode nil
	make-backup-files nil
	auto-save-default nil

	delete-by-moving-to-trash t)

  (defalias 'yes-or-no-p 'y-or-n-p)

  (setq completion-cycle-threshold 3)
  (setq tab-always-indent 'complete)

  (xterm-mouse-mode t)
  (mouse-wheel-mode t)
  (global-set-key [mouse-4] '(lambda () (interactive) (scroll-down 3)))
  (global-set-key [mouse-5] '(lambda () (interactive) (scroll-up 3)))

  ;; clipboard
  (defun copy-from-osx ()
    (shell-command-to-string "pbpaste"))

  (defun paste-to-osx (text &optional push)
    (let ((process-connection-type nil))
    (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
      (process-send-string proc text)
      (process-send-eof proc))))

  (setq interprogram-cut-function 'paste-to-osx)
  (setq interprogram-paste-function 'copy-from-osx)
  )

(use-package dired
  :custom
  (dired-do-revert-buffer t)
  (dired-recursive-copies 'always)
  (dired-isearch-filenames t))

(use-package exec-path-from-shell
  :straight t
  :config
  (exec-path-from-shell-initialize))

(use-package doom-themes
  :straight t
  :config
  (setq doom-themes-enable-bold t
	doom-themes-enable-italic t)
  (load-theme 'doom-one t))

(use-package doom-modeline
  :straight t
  :init
  (doom-modeline-mode 1))

(use-package nerd-icons
  :straight t)

(use-package nerd-icons-dired
  :straight t
  :after nerd-icons
  :commands (nerd-icons-dired-mode)
  :hook ((dired-mode . nerd-icons-dired-mode)))

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

(use-package undo-tree
  :straight t
  :custom
  (undo-tree-auto-save-history nil)
  :config
  (global-undo-tree-mode))

(use-package flymake
  :straight t)

(use-package flymake-diagnostic-at-point
  :straight t)

(use-package flymake-popon
  :straight (flymake-popon :type git :repo "https://codeberg.org/akib/emacs-flymake-popon.git")
  :config
  (global-flymake-popon-mode))

(use-package vertico
  :straight t
  :init
  (vertico-mode))

(use-package marginalia
  :straight t
  :init
  (marginalia-mode))

(use-package consult
  :straight t
  :defer t
  :custom
  (consult-preview-raw-size 1024000)
  (consult-preview-max-size 1024000)
  :config
  (consult-customize
   consult-buffer :preview-key "M-."))

(use-package consult-ghq
  :straight t
  :defer t
  :commands (consult-ghq-find)
  :bind (("C-c C-g" . consult-ghq-find)))

(use-package corfu
  :straight (:files (:defaults "extensions/*"))
  :custom
  (corfu-auto t)
  (corfu-auto-delay 0)
  (completion-styles '(basic))
  (corfu-popupinfo-delay nil)
  :config
  (global-corfu-mode))

(use-package corfu-terminal
  :straight t
  :after corfu
  :config
  (corfu-terminal-mode +1))

(use-package kind-icon
  :straight t
  :after corfu
  :custom
  (kind-icon-default-face 'corfu-default)
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(use-package orderless
  :straight t
  :defer t
  :custom
  (completion-styles '(orderless basic)))

(use-package treemacs
  :straight t
  :defer t
  :config
  (treemacs-indent-guide-mode t)
  (treemacs-fringe-indicator-mode t)
  (treemacs-filewatch-mode t)
  (treemacs-git-commit-diff-mode t))

(use-package treemacs-nerd-icons
  :straight t
  :after treemacs
  :config
  (treemacs-load-theme "nerd-icons"))

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

(use-package volatile-highlights
  :straight t
  :custom-face
  (vhl/default-face ((nil (:foreground "#FF3333" :background "#FFCDCD"))))
  :config
  (volatile-highlights-mode t)

  (vhl/define-extension 'undo-tree 'undo-tree-yank 'undo-tree-mode)
  (vhl/install-extension 'undo-tree))

(use-package highlight-indent-guides
  :disabled t
  :straight t
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
  :custom ((highlight-indent-guides-method 'character)
	   (highlight-indent-guides-bitmap-function #'my/highlight-indent-guides--bitmap-line)
	   (highlight-indent-guides-auto-character-face-perc 40)
	   (highlight-indent-guides-auto-top-character-face-perc 100)
	   (highlight-indent-guides-auto-enabled t)
	   (highlight-indent-guides-responsive t))
  :hook ((prog-mode . highlight-indent-guides-mode)))

(use-package dimmer
  :straight t
  :hook ((emacs-startup . dimmer-mode))
  :custom
  (dimmer-fraction 0.6)
  :config
  (dimmer-configure-which-key)
  (dimmer-configure-hydra)
  (dimmer-configure-posframe)
  (dimmer-configure-magit))

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

(use-package smartparens
  :straight t
  :config
  (require 'smartparens-config)

  (smartparens-global-mode t))

(use-package which-key
  :straight t
  :config
  (which-key-mode)
  (which-key-setup-side-window-bottom))

(use-package projectile
  :straight t
  :bind (:map projectile-mode-map
	      ("M-p" . projectile-command-map))
  :hook ((emacs-startup . projectile-mode))
  :custom
  (projectile-generic-dommand "fd . -0 --type f --color=never"))

;;
;; git
;;

(use-package blamer
  :straight t
  :config
  (global-blamer-mode))

(use-package git-timemachine
  :straight t)

(use-package git-modes
  :straight t)

(use-package git-gutter
  :straight t
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
;; programming
;;

(use-package yasnippet
  :straight t
  :config
  :config
  (yas-global-mode 1))

(use-package treesit-auto
  :straight t
  :config
  (setq treesit-auto-install 'prompt)
  (global-treesit-auto-mode))

(use-package eglot
  :config
  (defun deno-project-p ()
    "Predicate for determining if the open project is a Deno one."
    (let ((p-root (cdr (project-current))))
      (file-exists-p (concat p-root "deno.json"))))

  (defun node-project-p ()
    "Predicate for determining if the open project is a Node one."
    (let ((p-root (cdr (project-current))))
      (file-exists-p (concat p-root "package.json"))))

  (defun es-server-program (_)
    "Decide which server to use for ECMA Script based on project characteristics."
    (cond ((deno-project-p) '("deno" "lsp" :initializationOptions (:enable t :lint t)))
          ((node-project-p) '("typescript-language-server" "--stdio"))
          (t                nil)))

  (add-to-list 'eglot-server-programs '((js-mode typescript-mode) . es-server-program)))

;;
;; modes
;;

(use-package markdown-mode
  :straight t
  :mode (("README\\.md\\'" . gfm-mode)
	 ("\\.mdx\\'" . markdown-mode))
  :init
  (setq markdown-command "multimarkdown"))

(use-package paredit
  :straight t
  :hook ((emacs-lisp-mode . enable-paredit-mode)))

(use-package typescript-mode
  :straight t
  :mode (("\\.ts\\'" . typescript-mode)
	 ("\\.tsx\\'" . tsx-ts-mode))
  :hook ((typescript-mode . eglot-ensure))
  :custom
  (typescript-indent-level 2)
  (typescript-tsx-indent-offset 2)
  (typescript-ts-mode typescript-mode-hook))

(use-package javascript-mode
  :straight t
  :custom
  (js-indent-level 2))

(use-package yaml-mode
  :straight t)

(use-package groovy-mode
  :straight t)

(use-package eglot-java
  :straight t
  :defer t
  :hook ((java-ts-mode . eglot-java-mode))
  :init
  (setq lombok-path (expand-file-name "~/.m2/repository/org/projectlombok/lombok/1.18.26/lombok-1.18.26.jar"))
  :custom
  (eglot-java-eclipse-jdt-args
   `(
     "-noverify"
     "-Xmx1G"
     "-XX:+UseG1GC"
     "-XX:+UseStringDeduplication"
     ,(concat "-javaagent:" lombok-path)
     ,(concat "-Xbootclasspath/a:" lombok-path))))

(use-package org-mode
  :straight t
  :custom
  (system-time-locate nil)
  (org-startup-with-inline-images t)
  (org-ellipsis " ▼")
  (org-fontify-quote-and-verse-blocks t)
  (org-use-speed-commands t)

  (org-display-custom-times t)
  (org-image-actual-width nil)

  (org-agenda-tags-column 0)
  (org-agenda-block-separator ?─)
  (org-agenda-time-grid
   '((daily today require-timed)
     (800 1000 1200 1400 1600 1800 2000)
     " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄")
   org-agenda-current-time-string
   "⭠ now ─────────────────────────────────────────────────")

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
  :hook ((org-mode . (lambda ()
		       (require 'org-bars)
		       (org-bars-mode)))))

(use-package org-modern
  :straight t
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

  (setq org-ditaa-jar-path "/opt/homebrew/Cellar/ditaa/0.11.0_1/libexec/ditaa-0.11.0-standalone.jar")
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((ditaa . t)))
  )

(use-package org-modern-indent
  :straight (org-modern-indent :type git
			       :host github
			       :repo "jdtsmith/org-modern-indent")
  :defer t
  :config
  (add-hook 'org-mode-hook #'org-modern-indent-mode 90))
