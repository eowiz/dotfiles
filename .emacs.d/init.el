;; -*- coding:utf-8 lexical-binding: t -*-

;; <leaf-install-code>
(eval-and-compile
  (customize-set-variable
   'package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                       ("melpa" . "https://melpa.org/packages/")
                       ("org" . "https://orgmode.org/elpa/")))
  (package-initialize)
  (unless (package-installed-p 'leaf)
    (package-refresh-contents)
    (package-install 'leaf))

  (leaf leaf-keywords
    :ensure t
    :init
    ;; optional packages if you want to use :hydra, :el-get, :blackout,,,
    (leaf hydra :ensure t)
    (leaf el-get :ensure t)
    (leaf blackout :ensure t)

    :config
    ;; initialize leaf-keywords.el
    (leaf-keywords-init)))
;; </leaf-install-code>

(leaf diminish :ensure t)
(leaf el-get :ensure t)

(eval-when-compile (require 'cl-lib))

(leaf exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-initialize))

(leaf cus-start
  :doc "define customization properties of builtins"
    :tag "builtin" "internal"
    :custom `((indent-tabs-mode . nil)
              (make-backup-files . nil)
              (auto-save-default . nil)
              (suggest-key-bindings . nil)
              (delete-by-moving-to-trash . t)
              (custom-file . ,(locate-user-emacs-file "custom.el")))
    :config
    (defalias 'yes-or-no-p 'y-or-n-p)

    (show-paren-mode t))

(leaf mac
  :when (eq system-type 'darwin)
  :custom ((dired-use-ls-dired . t))
  :config
  (let ((gls (executable-find "gls")))
    (when gls
      (setq insert-directory-program gls
            dired-listing-switches "-aBhl --group-directories-first")))

  (leaf key-config
    :custom ((mac-command-modifier . 'alt)
             (mac-option-modifier . 'meta)
             (default-directory . "~/")
             (command-line-default-directory . "~/"))))

(leaf transient
  :ensure t)

;;
;; Look & Feel
;;

(modify-all-frames-parameters
 '((right-divider-width . 30)
   (internal-border-width . 30)))

(leaf moom
  :ensure t
  :custom ((moom-use-font-module . nil)
           ;; (top bottom left right)
           (moom-user-margin . '(10 70 10 0)))
  :bind (("C-c f f l" . moom-fill-left)
         ("C-c f f r" . moom-fill-right)
         ("C-c f f s" . moom-fill-screen)
         (:moom-mode-map
          ("C-c o " . moom-transient-dispatch)))
  :require moom moom-transient
  :config
  (moom-mode)

  (moom-fill-left))

(leaf modus-themes
  :ensure t
  :custom ((modus-themes-italic-constructs . t)
           (modus-themes-bold-constructs . t)
           (modus-themes-region . '(bg-only no-extend))
           ((modus-themes-syntax . '(faint green-strings))))
  :config
  (modus-themes-load-themes)
  (modus-themes-load-operandi))

(leaf moody
  :ensure t
  :disabled t
  :custom ((x-underline-at-descent-line . t))
  :config
  (let ((line (face-attribute 'mode-line :underline)))
    (set-face-attribute 'mode-line          nil :overline   line)
    (set-face-attribute 'mode-line-inactive nil :overline   line)
    (set-face-attribute 'mode-line-inactive nil :underline  line)
    (set-face-attribute 'mode-line          nil :box        nil)
    (set-face-attribute 'mode-line-inactive nil :box        nil)
    (set-face-attribute 'mode-line-inactive nil :background "#f9f2d9"))
  (moody-replace-mode-line-buffer-identification)
  (moody-replace-vc-mode)
  (moody-replace-eldoc-minibuffer-message-function))

;;
;; Modeline
;;

(leaf nano-modeline
  :ensure t
  :config
  (nano-modeline-mode))

(leaf mlscroll
  :ensure t
  :disabled t
  :config
  (custom-set-variables
   '(mlscroll-in-color "#FFA07A") ;; light coral
   '(mlscroll-out-color "#FFFFE0")
   '(mlscroll-width-chars 12))
  :global-minor-mode mlscroll)

(leaf hide-mode-line
  :ensure t
  :hook ((neotree-mode-hook . hide-mode-line-mode)
         (imenu-list-minor-mode-hook . hide-mode-line-mode)
         (minimap-mode-hook . hide-mode-line-mode)))

;;
;; Dashboard
;;

;; https://github.com/emacs-dashboard/emacs-dashboard

(leaf dashboard
  :ensure t
  :custom ((dashboard-banner-logo-title . "")
           (dashboard-startup-banner . nil)
           (dashboard-set-footer . nil)
           (dashboard-center-content . t)
           (dashboard-show-shortcuts . t)
           (dashboard-set-heading-icons . t)
           (dashboard-set-file-icons . t))
  :config
  (dashboard-setup-startup-hook))

;;
;; Scroll
;;

(leaf smooth-scroll
  :disabled t
  :diminish ""
  :ensure t
  :global-minor-mode t)

(leaf scroll
  :custom
  (scroll-margin . 0)
  (scroll-conservatively . 100000)
  (scroll-preserve-screen-position . t))

;;
;; Icons
;;

(leaf all-the-icons
  :ensure t
  :if (display-graphic-p))

(leaf prettify-symbols
  :diminish ""
  :hook org-mode-hook elm-mode-hook)

;;
;; Tab
;; 

(leaf centaur-tabs
  :disabled t
  :ensure t
  :custom ((centaur-tabs--buffer-show-groups . nil)
           (centaur-tabs-height . 32)
           (centaur-tabs-cycle-scope . 'tabs)
           (centaur-tabs-style . "wave")
           (centaur-tabs-set-bar . 'under)
           (centaur-tabs-set-icons . t)
           (centaur-tabs-gray-out-icons . 'buffer)
           (centaur-tabs-set-close-button . nil)
           (centaur-tabs-set-modified-marker . t)
           ;; When Noto Sans Symbols 2 cloud not be installed
           ;; (centaur-tabs-modified-marker . "*")
           )
  :config
  (centaur-tabs-mode t)
  (centaur-tabs-group-by-projectile-project))

(leaf autorevert
  :diminish auto-revert
  :global-minor-mode global-auto-revert-mode)

(leaf vterm
  :ensure t)

;; org-mode

(leaf org
  :bind (("<f12>" . org-agenda))
  :setq-default ((org-enforce-todo-dependencies . t))
  :custom (
           (org-src-tab-acts-natively . t)
           (org-src-preserve-indentation . t)
           (org-edit-src-content-indentation . 0)
           ;; (org-agenda-files . '("~/org"))
           (org-src-fontify-natively . t)
           ;; (org-adapt-indentation . t)
           (org-edit-src-content-indentation . 0)
           )
  )

(leaf org-modern
  :ensure t
  :hook ((org-mode-hook . (lambda () (org-modern-mode 1)))
         (org-agenda-finalize-hook . #'org-modern-agenda))
  :config
  (dolist (face '(window-divider
                  window-divider-first-pixel
                  window-divider-last-pixel))
    (face-spec-reset-face face)
    (set-face-foreground face (face-attribute 'default :background)))
  (set-face-background 'fringe (face-attribute 'default :background))

  (setq
   ;; Edit settings
   org-auto-align-tags nil
   org-tags-column 0
   org-catch-invisible-edits 'show-and-error
   org-special-ctrl-a/e t
   org-insert-heading-respect-content t

   ;; Org styling, hide markup etc.
   org-hide-emphasis-markers t
   org-pretty-entities t
   org-ellipsis "…"

   ;; Agenda styling
   org-agenda-tags-column 0
   org-agenda-block-separator ?─
   org-agenda-time-grid
   '((daily today require-timed)
     (800 1000 1200 1400 1600 1800 2000)
     " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄")
   org-agenda-current-time-string
   "⭠ now ─────────────────────────────────────────────────"))

;;
;; IME
;;

(leaf skk
  :diminish ""
  :ensure ddskk
  :bind (("C-x C-j" . skk-mode))
  :custom ((default-input-method . "japanese-skk")
           (skk-large-jisyo . "~/.emacs.d/dict/SKK-JISYO.L")
           (skk-status-indicator . nil))
  :pre-setq
  (skk-byte-compile-init-file . t)
  :hook ((text-mode-hook . (lambda () (skk-mode) (skk-latin-mode-on)))
         (prog-mode-hook . (lambda () (skk-mode) (skk-latin-mode-on))))
  :config
  (leaf ddskk-posframe
    :diminish ""
    :ensure t
    :global-minor-mode t))

(leaf orderless
    :ensure t
    :init (setq completion-styles '(orderless)
                completion-category-defaults 'nil
                completion-category-overrides '((file (styles partial-completion)))))

(leaf vertico
  :ensure t
  :custom ((vertico-count . 20)
           (vertico-cycle . t))
  :init
  (vertico-mode)

  :config
  (leaf emacs
    :init
    ;; Add prompt indicator to `completing-read-multiple'.
    ;; Alternatively try `consult-completing-read-multiple'.
    (defun crm-indicator (args)
      (cons (concat "[CRM] " (car args)) (cdr args)))
    (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

    ;; Do not allow the cursor in the minibuffer prompt
    (setq minibuffer-prompt-properties
          '(read-only t cursor-intangible t face minibuffer-prompt))
    (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

    ;; Emacs 28: Hide commands in M-x which do not work in the current mode.
    ;; Vertico commands are hidden in normal buffers.
    ;; (setq read-extended-command-predicate
    ;;       #'command-completion-default-include-p)

    ;; Enable recursive minibuffers
    (setq enable-recursive-minibuffers t)))

(defun consult-thing-at-point (&optional at-point)
  "Consult-line uses things-at-point."
  (interactive "P")
  (consult-line (thing-at-point 'symbol)))

(leaf consult
  :ensure t
  :bind (("C-s" . consult-line)
         ("M-s M-s" . consult-thing-at-point)
         ("C-c , g" . consult-ripgrep)
         ([remap goto-line] . consult-goto-line))
  :custom `((consult-preview-raw-size . 1024000)
            (consult-preview-key . ,(kbd "M-."))))

(leaf marginalia
  :ensure t
  :init
  (marginalia-mode))

(leaf embark
  :ensure t
  :bind (("C-." . embark-act)
         ("C-;" . embark-dwim)
         ("C-h B" . embark-bindings))
  :init
  (setq prefix-help-command #'embark-prefix-help-command)

  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(leaf embark-consult
  :ensure t
  :after embark consult
  :leaf-defer nil
  :hook ((embark-collect-mode . consult-preview-at-point-mode)))

(leaf consult-ghq
  :ensure t
  :bind (("C-c C-g" . consult-ghq-find)))

(leaf anzu
  :diminish ""
  :ensure t
  :bind (([remap query-replace] . 'anzu-query-replace)
         ([remap query-replace-regex] . 'anzu-query-replace-regex))
  :custom ((anzu-replace-threshold . 1000)
           (anzu-search-threshold . 1000))
  :config
  (copy-face 'mode-line 'anzu-mode-line))

(leaf affe
  :ensure t
  :disabled t
  :bind (("C-c a s" . affe-grep)
         ("C-c a f" . affe-find))
  :init (setq affe-highlight-function 'orderless-highlight-matches
              affe-regexp-function 'orderless-pattern-compiler))

;;
;; Highlights
;;

(leaf volatile-highlights
  :diminish ""
  :ensure t
  :global-minor-mode volatile-highlights-mode)

(leaf highlight-indent-guides
  :disabled t
  :diminish ""
  :ensure t
  :hook prog-mode-hook yaml-mode-hook
  :custom ((highlight-indent-guides-auto-enabled . t)
           (highlight-indent-guides-responsive . t)
           (highlight-indent-guides-method . 'character)))

(leaf hl-line-mode
  :global-minor-mode global-hl-line-mode)

;;
;; History
;;

(leaf savehist
   :ensure t
   :init
   (savehist-mode))

(leaf prescient
  :ensure t
  :custom `((prescient-aggresive-file-save . t)
            (prescient-save-file . ,(expand-file-name "~/.emacs.d/prescient-save.el")))
  :global-minor-mode prescient-persist-mode)

(leaf undohist
  :ensure t
  :require t
  :config
  (undohist-initialize))

(leaf undo-tree
  :diminish ""
  :ensure t
  :custom ((undo-tree-auto-save-history . nil))
  :global-minor-mode global-undo-tree-mode)

;;
;; projectile
;;

(leaf projectile
  :ensure t
  :config
  (projectile-mode +1))

(leaf consult-projectile
  :ensure t
  :bind (("C-c p f" . consult-projectile-find-file)))

(leaf imenu-list
  :ensure t
  :bind (("C-'" . imenu-list-smart-toggle)))

(leaf neotree
  :ensure t
  :bind (("M-\\" . neotree-projectile-toggle))
  :commands (neotree-show neotree-hide neotree-dir neotree-find)
  :custom ((neo-smart-open . t)
           (neo-show-hidden-files . t)
           (neo-create-file-auto-open . t)
           (neo-window-fixed-size . nil)
           (neo-window-width . 40))
  :config
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow))

  (defun neotree-projectile-toggle ()
    (interactive)
    (let ((project-dir
           (ignore-errors
       ;;; Pick one: projectile or find-file-in-project
             (projectile-project-root)
             ))
          (file-name (buffer-file-name))
          (neo-smart-open t))
      (if (and (fboundp 'neo-global--window-exists-p)
               (neo-global--window-exists-p))
          (neotree-hide)
        (progn
          (neotree-show)
          (if project-dir
              (neotree-dir project-dir))
          (if file-name
              (neotree-find file-name)))))))

(leaf corfu
  :ensure t
  :pre-setq ((tab-always-indent . t)
             (corfu-cycle . t)
             (corfu-auto . t)
             (corfu-auto-prefix . 3)
             (corfu-popupinfo-delay . 0))
  :global-minor-mode global-corfu-mode
  :config

  (corfu-popupinfo-mode)

  (leaf cape
    :ensure t
    :init
    (add-to-list 'completion-at-point-functions #'cape-file)
    (add-to-list 'completion-at-point-functions #'cape-tex)
    (add-to-list 'completion-at-point-functions #'cape-dabbrev)
    (add-to-list 'completion-at-point-functions #'cape-keyword)
    (add-to-list 'completion-at-point-functions #'cape-symbol))

  (leaf kind-icon
    :ensure t
    :require t
    :after corfu
    :custom ((kind-icon-default-face . 'corfu-default))
    :config
    (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter)))

(leaf eww
  :bind (:eww-mode-map
         ("h" . backward-char)
         ("j" . next-line)
         ("k" . previous-line)
         ("l" . forward-char)
         ("J" . view-scroll-line-forward)
         ("K" . view-scroll-line-forward)
         ("[" . eww-back-url)
         ("]" . eww-forward-url))
  :custom ((eww-search-prefix . "https://www.google.co.jp/search?kl=jp-jp&k1=-1&kf=-1&q="))
  :config
  (defun eww-mode-hook--rename-buffer ()
    "Rename eww browser's buffer so sites open in new page."
    (rename-buffer "eww" t))
  (add-hook 'eww-mode-hook 'eww-mode-hook--rename-buffer))

(leaf smartparens
  :diminish ""
  :ensure t
  :require smartparens-config
  :hook ((prog-mode-hook . turn-on-smartparens-mode)
         (org-mode-hook . turn-on-smartparens-mode)))

(leaf tree-sitter
  :ensure t
  :hook ((tree-sitter-after-on-hook . tree-sitter-hl-mode))
  :global-minor-mode global-tree-sitter-mode

  :config
  (leaf tree-sitter-langs
    :ensure t
    :config
    (tree-sitter-require 'c)
    (tree-sitter-require 'rust)
    (tree-sitter-require 'typescript)
    (tree-sitter-require 'java)
    (tree-sitter-require 'elm)))

(leaf eldoc
  :diminish ""
  :config
  (defun ad:eldoc-message (f &optional string)
    (unless (active-minibuffer-window)
      (funcall f string)))
  (advice-add 'eldoc-message :around #'ad:eldoc-message))

(leaf display-fill-column-indicator
  :hook git-commit-mode-hook
  :custom
  (display-fill-column-indicator-column . 50))

(leaf rainbow-mode
  :diminish ""
  :ensure t
  :hook prog-mode-hook)

;; (leaf rainbow-delimiters
;;   :diminish ""
;;   :ensure t
;;   :hook prog-mode-hook)

(leaf flycheck
  :diminish ""
  :ensure t
  :global-minor-mode global-flycheck-mode)

(leaf magit
  :diminish ""
  :ensure t
  :custom ((magit-display-buffer-function . #'magit-display-buffer-fullframe-status-v1)))

(leaf git-modes
  :ensure t)

(leaf git-gutter
  :diminish ""
  :ensure t
  :custom
  ((git-gutter:unchanged-sign . " ")
   (git-gutter:modified-sign  . " ")
   (git-gutter:added-sign     . " ")
   (git-gutter:deleted-sign   . " "))
  :custom-face
  `((git-gutter:unchanged . '((t (:background ,(face-attribute 'line-number :background)))))
    (git-gutter:modified  . '((t (:background "#f1fa8c"))))
    (git-gutter:added     . '((t (:background "#50fa7b"))))
    (git-gutter:deleted   . '((t (:background "#ff79c6")))))
  :global-minor-mode global-git-gutter-mode)

(leaf lsp-mode
  :ensure t
  :pre-setq ((lsp-keymap-prefix . "M-l")
             (lsp-idle-delay . 0.5)
             (lsp-log-io . nil)
             (lsp-completion-provider . :none))
  :custom ((lsp-document-sync-method lsp--sync-incremental)))

(leaf lsp-ui
  :ensure t)

(leaf dumb-jump
  :ensure t
  :config

  (defhydra dumb-jump-hydra (:color blue :columns 3)
    "Dumb Jump"
    ("j" dumb-jump-go "Go")
    ("o" dumb-jump-go-other-window "Other window")
    ("e" dumb-jump-go-prefer-external "Go external")
    ("x" dumb-jump-go-prefer-external-other-window "Go external other window")
    ("i" dumb-jump-go-prompt "Prompt")
    ("l" dumb-jump-quick-look "Quick look")
    ("b" dumb-jump-back "Back")))

(leaf yaml-mode
  :ensure t)

(leaf web-mode
  :ensure t
  :mode ("\\.html?\\'" "\\.njk\\'")
  :custom ((web-mode-markup-indent-offset . 2)))

(leaf js-mode
  :custom ((js-indent-level . 2)))

(leaf typescript-mode
  ;; :ensure t
  :el-get emacs-typescript/typescript.el
  :mode ("\\.ts?\\'" "\\.tsx?\\'")
  :custom ((indent-tabs-mode . nil)
           (typescript-indent-level . 2)))

(leaf elm-mode
  :ensure t
  :hook ((elm-mode-hook . elm-format-on-save-mode)
         (elm-mode-hook . (lambda () (push '("|>" . ?▷) prettify-symbols-alist)
                            (push '("<|" . ?◁) prettify-symbols-alist)
                            (push '("->" . ?→) prettify-symbols-alist)))))

(leaf fsharp-mode
  :ensure t)

(leaf java
  :config
  (let* ((lombok-version "1.18.24")
         (lombok-jar-dir (expand-file-name (format "~/.m2/repository/org/projectlombok/lombok/%s/" lombok-version)))
         (lombok-jar-path (expand-file-name (format "~/.m2/repository/org/projectlombok/lombok/%s/lombok-%s.jar" lombok-version lombok-version)))
         (lombok-jar-url (format "https://repo1.maven.org/maven2/org/projectlombok/lombok/%s/lombok-%s.jar" lombok-version lombok-version)))
    (progn
      (if (not (file-exists-p lombok-jar-path))
          (progn
            (make-directory lombok-jar-dir t)
            (url-copy-file lombok-jar-url lombok-jar-path)))

      (leaf lsp-java
        :ensure t
        :custom ((lsp-java-format-enabled . t)
                 (lsp-java-vmargs . `("-Xmx1G"
                                      "-XX:+UseG1GC"
                                      "-XX:+UseStringDeduplication"
                                      ,(concat "-javaagent:" lombok-jar-path)
                                      ,(concat "-Xbootclasspath/a:" lombok-jar-path))))
        :hook ((java-mode-hook . lsp)
               (java-mode-hook . (lambda () (setq c-basic-offset 2))))))))

(leaf gradle-mode
  :ensure t)

(leaf groovy-mode
  :ensure t)

(leaf writeroom-mode
  :ensure t)

(defun open-init-org ()
  "Toggle current buffer between init.org."
  (interactive)
  (let ((path (buffer-file-name)))
    (if (equal path (expand-file-name "~/.emacs.d/inits/init.org"))
        (switch-to-buffer (other-buffer))
      (find-file "~/.emacs.d/init.el"))))

(leaf custom-key-bindings
  :bind (("M-SPC" . open-init-org)))

;;; init.el ends here
