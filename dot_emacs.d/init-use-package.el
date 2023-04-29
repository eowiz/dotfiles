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

  (global-hl-line-mode))

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

;; vertico
(use-package vertico
  :straight t
  :defer t
  :init
  (vertico-mode))

(use-package consult
  :straight t
  :defer t
  :commands (consult-line consult-customize)
  :bind (("C-c M-x" . consult-mode-command)
	 ("C-c k" . consult-kmacro)
	 :map isearch-mode-map
	 ("M-l" . consult-line)
	 ("M-g" . consult-ripgrep))
  :custom
  (consult-preview-raw-size 1024000)
  (consult-preview-max-size 1024000))

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
  :defer t
  :init
  (define-prefix-command 'my/tab-bar-map)
  :bind-keymap
  ("C-z" . my/tab-bar-map)
  :bind (:map my/tab-bar-map
	      ("n" . tab-bar-switch-to-next-tab)
	      ("p" . tab-bar-switch-to-prev-tab)
	      ("r" . tab-bar-switch-to-recent-tab)
	      ("c" . tab-bar-new-tab)
	      ("x" . tab-bar-close-tab))
  :config
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

;;
;; org-mode
;;
(use-package org-mode
  :straight t
  :defer t)

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
  :defer t)

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

;; application-framework

(use-package eaf
  :straight (eaf :host github
		 :repo "emacs-eaf/emacs-application-framework"
		 :files ("*.el" "*.py" "*.json"
			 "core" "swaymsg-treefetch" "extension" "app")
		 :includes (eaf-pdf-viewer eaf-browser eaf-git eaf-terminal)
		 :pre-build ("python3" "install-eaf.py" "--install" "pdf-viewer" "browser" "git" "terminal" "--ignore-sys-deps"))
  :defer 1)

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
  :defer 3)
