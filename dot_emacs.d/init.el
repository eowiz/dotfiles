;;; init.el --- eowiz's init.el -*- lexical-binding: t -*-
;;; Commentary:

;; see: https://github.com/takeokunn/.emacs.d

;;; Code:



(defconst my/saved-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

(defvar my/delayed-priority-high-configurations '())
(defvar my/delayed-priority-high-configuration-timer nil)

(defvar my/delayed-priority-low-configurations '())
(defvar my/delayed-priority-low-configuration-timer nil)

(add-hook 'emacs-startup-hook
	  (lambda ()
            (setq my/delayed-priority-high-configuration-timer
                  (run-with-timer
                   0.1 0.001
                   (lambda ()
                     (if my/delayed-priority-high-configurations
                         (let ((inhibit-message t))
                           (eval (pop my/delayed-priority-high-configurations)))
                       (progn
                         (cancel-timer my/delayed-priority-high-configuration-timer))))))
            (setq my/delayed-priority-low-configuration-timer
                  (run-with-timer
                   0.3 0.001
                   (lambda ()
                     (if my/delayed-priority-low-configurations
                         (let ((inhibit-message t))
                           (eval (pop my/delayed-priority-low-configurations)))
                       (progn
                         (cancel-timer my/delayed-priority-low-configuration-timer))))))))

(defmacro with-delayed-execution-priority-high (&rest body)
  (declare (indent 0))
  `(setq my/delayed-priority-high-configurations
         (append my/delayed-priority-high-configurations ',body)))

(defmacro with-delayed-execution (&rest body)
  (declare (indent 0))
  `(setq my/delayed-priority-low-configurations
         (append my/delayed-priority-low-configurations ',body)))

;;;###autoload
(defun autoload-if-found (functions file &optional docstring interactive type)
  "set autoload iff. FILE has found."
  (when (locate-library file)
    (dolist (f functions)
      (autoload f file docstring interactive type))
    t))

;; ------------------------------------------------------------------------------------------

(setq custom-file (locate-user-emacs-file "custom.el"))

(eval-and-compile
  (setq byte-compile-warnings '(cl-functions))
  (require 'cl-lib nil t))

;; ------------------------------------------------------------------------------------------

(eval-when-compile
  (add-to-list 'load-path (locate-user-emacs-file "local-packages"))
  (require 'minima))

;; ------------------------------------------------------------------------------------------

;; Package Manager
(eval-when-compile
 (unless (file-directory-p (locate-user-emacs-file "elpa/el-clone"))
   (package-vc-install "https://github.com/eowiz/el-clone.git")))

(eval-when-compile
 (add-to-list 'load-path (locate-user-emacs-file "elpa/el-clone"))
 (require 'el-clone))

;; NativeComp
(with-eval-after-load 'comp
  (setq native-comp-async-jobs-number 8)
  (setq native-comp-speed 3))

;;
;; packages
;;

;; compat
(minima
 :clone "emacs-compat/compat")

;; dash
(minima
 :clone "magnars/dash.el")

;; s
(minima
 :clone "magnars/s.el")

;; f
(minima
 :clone "rejeep/f.el")

;; ht
(minima
 :clone "Wilfred/ht.el")

;; pfuture
(minima
 :clone "Alexander-Miller/pfuture")

;; shrink-path
(minima
 :clone "zbelial/shrink-path.el")

;; svg-lib
(minima
 :clone "rougier/svg-lib")

;; request
(minima
 :clone "tkf/emacs-request")

;; spinner
(minima
 :clone "Malabarba/spinner.el")

;; hydra
(minima
 :clone "abo-abo/hydra")

;; yasnippet
(minima
 :clone "joaotavora/yasnippet"
 :priority 'low)

(with-delayed-execution-priority-high
  (autoload-if-found '(yas-global-mode) "yasnippet" nil t)
  
  (yas-global-mode 1))

(with-delayed-execution
  (setq indent-tabs-mode nil
	make-backup-files nil
	auto-save-default nil
	suggest-key-bindings nil
	delete-by-moving-to-trash t)
  (setq insert-directory-program "gls")
  (defalias 'yes-or-no-p 'y-or-n-p))

;; exec-path-from-shell
(minima
 :clone "purcell/exec-path-from-shell")

(with-delayed-execution-priority-high
  (add-to-list 'load-path (locate-user-emacs-file "el-clone/exec-path-from-shell"))

  (when (require 'exec-path-from-shell)
    (exec-path-from-shell-initialize)))

;; nord-theme
;; (eval-when-compile
;;   (el-clone :repo "nordtheme/emacs"
;; 	    :load-paths `(,(locate-user-emacs-file "el-clone/emacs"))))

;; (with-delayed-execution-priority-high
;;   (add-to-list 'load-path (locate-user-emacs-file "el-clone/emacs"))

;;   (when (require 'nord-theme)
;;     (load-theme 'nord t)))

;; everforest
;; (eval-when-compile
;;   (el-clone :repo "Theory-of-Everything/everforest-emacs"))

;; (with-delayed-execution-priority-high
;;   (add-to-list 'load-path (locate-user-emacs-file "el-clone/everforest-emacs"))

;;   (when (require 'everforest-hard-dark-theme)
;;     (load-theme 'everforest-hard-dark t)))

;; modus theme
(minima
 :clone "protesilaos/modus-themes")

(with-delayed-execution-priority-high
  ;; (add-to-list 'load-path (locate-user-emacs-file "el-clone/modus-themes"))

  (when (require 'modus-operandi-theme)
    (load-theme 'modus-operandi-tinted t))
  )

;; all-the-icons
(minima
 :clone "domtronn/all-the-icons.el")

;; doom-modeline

(minima
 :clone "seagle0128/doom-modeline")

(with-delayed-execution-priority-high
  (add-to-list 'load-path (locate-user-emacs-file "el-clone/doom-modeline"))

  (autoload-if-found '(doom-modeline-mode) "doom-modeline" nil t)

  (doom-modeline-mode 1))

(with-eval-after-load 'doom-modeline
  (setq doom-modeline-buffer-file-name-style 'truncate-with-project)
  (setq doom-modeline-icon t)
  (setq doom-modeline-major-mode-icon t)
  (setq doom-modeline-minor-modes nil))

;; mini-frame
;; (minima
;;  :clone "muffinmad/emacs-mini-frame")

;; (with-delayed-execution-priority-high
;;   (autoload-if-found '(mini-frame-mode) "mini-frame" nil t)

;;   (mini-frame-mode)

;;   (custom-set-variables
;;    '(mini-frame-show-parameters
;;      '((top . 1.0)
;;        (width . 1.0)
;;        (left . 0)))))

;; scroll

(with-delayed-execution
  (pixel-scroll-precision-mode t)

  (setq scroll-margin 0)
  (setq scroll-conservatively 100000)
  (setq scroll-preserve-screen-position t))

;; dirvish
(minima
 :clone "alexluigit/dirvish")

(with-delayed-execution-priority-high
  (add-to-list 'load-path (locate-user-emacs-file "el-clone/dirvish"))
  (add-to-list 'load-path (locate-user-emacs-file "el-clone/dirvish/extensions"))

  (autoload-if-found '(dirvish-override-dired-mode) "dirvish" nil t)
  (autoload-if-found '(dirvish-quick-access) "dirvish-quick-access" nil t)
  (autoload-if-found '(dirvish-emerge-mode dirvish-emerge-menu) "dirvish-emerge" nil t)
  (autoload-if-found '(dirvish-side) "dirvish-side" nil t)

  (dirvish-override-dired-mode)

  (define-key dirvish-mode-map (kbd "TAB") #'dirvish-subtree-toggle)
  (define-key dirvish-mode-map (kbd "a") #'dirvish-quick-access)
  (define-key dirvish-mode-map (kbd "M-e") #'dirvish-emerge-menu)

  (global-set-key (kbd "C-'") #'dirvish-side)

  (setq dired-mouse-drag-files t)
  (setq mouse-drag-and-drop-region-cross-program t)

  (setq dirvish-subtree-prefix "  ")
  (setq dirvish-subtree-always-show-state t)

  (setq dirvish-attributes
	'(vc-state subtree-state all-the-icons collapse file-time file-size))
  (setq dired-listing-switches
        "-l --almost-all --human-readable --group-directories-first --no-group")

  (setq dirvish-use-header-line 'global)
  (setq dirvish-header-line-height '(25 . 35))
  (setq dirvish-mode-line-height 25)
  (setq dirvish-header-line-format
	'(:left (path) :right (free-space))
	dirvish-mode-line-format
	'(:left (sort file-time " " file-size symlink) :right (omit yank index)))

  (setq dirvish-emerge-groups '(("READMD" (regex . "README"))
				("Emacs Lisp" (extensions "el"))))

  (setq dirvish-quick-access-entries
	`(("h" "~/"                      "Home")
	  ("e" ,user-emacs-directory     "Emacs user directory")
	  ("c" "~/.local/share/chezmoi/" "chezmoi")
	  ("o" "~/org"                   "Org")))
  )

;; ddskk

(minima
 :clone "skk-dev/ddskk")

(with-delayed-execution-priority-high
  (add-to-list 'load-path (locate-user-emacs-file "el-clone/ddskk"))
  ;; (autoload-if-found '(skk-mode) "skk" nil t)

  (require 'skk-autoloads)

  (setq skk-preload t)
  (global-set-key (kbd "C-x C-j") #'skk-mode)
  (global-set-key (kbd "C-x ^") #'skk-kakutei)
  (add-hook 'text-mode-hook (lambda () (skk-mode) (skk-latin-mode-on)))
  (add-hook 'prog-mode-hook (lambda () (skk-mode) (skk-latin-mode-on))))

(minima
 :clone "conao3/ddskk-posframe.el")

(with-delayed-execution-priority-high
  (add-to-list 'load-path (locate-user-emacs-file "el-clone/ddskk-posframe"))

  (autoload-if-found '(ddskk-posframe-mode) "ddskk-posframe" nil t)

  (with-eval-after-load 'skk
    (add-hook 'skk-mode-hook #'ddskk-posframe-mode)))

(minima
 :clone "tumashu/posframe")

;; anzu
(minima
 :clone "emacsorphanage/anzu")

(with-delayed-execution-priority-high
  (add-to-list 'load-path (locate-user-emacs-file "el-clone/anzu"))

  (autoload-if-found '(anzu-query-replace anzu-query-replace-regex anzu-mode-line) "anzu" nil t)

  (global-set-key [remap query-replace] #'anzu-query-replace)
  (global-set-key [remap query-replace-regexp] #'anzu-query-replace-regex)

  (setq anzu-replace-threshold 1000)
  (setq anzu-search-threshold 1000)

  (copy-face 'mode-line 'anzu-mode-line)

  (when (require 'anzu)
    (global-anzu-mode +1)))

;; which-key
(minima
 :clone "justbur/emacs-which-key")

(with-delayed-execution
  (add-to-list 'load-path (locate-user-emacs-file "el-clone/emacs-which-key"))

  (autoload-if-found '(which-key-mode which-key-setup-side-window-bottom) "which-key" nil t)

  (which-key-mode)
  (which-key-setup-side-window-bottom))

;; corfu
(minima
 :clone "minad/corfu")

(with-delayed-execution-priority-high
  (add-to-list 'load-path (locate-user-emacs-file "el-clone/corfu"))
  (add-to-list 'load-path (locate-user-emacs-file "el-clone/corfu/extensions"))

  (autoload-if-found '(corfu-popupinfo-mode corfu-complete) "corfu-popupinfo" nil t)

  (when (require 'corfu)
    (setq corfu-auto t)
    (setq corfu-cycle t)
    (setq tab-always-indent 'complete)
    (setq corfu-auto-prefix 2)
    (setq corfu-popupinfo-delay 0)

    (global-corfu-mode)

    ;; (corfu-popupinfo-mode)
    ))

;; cape
(minima
 :clone "minad/cape")

(with-delayed-execution
  (add-to-list 'load-path (locate-user-emacs-file "el-clone/cape"))

  (autoload-if-found '(cape-super-capf cape-file cape-tex cape-dabbrev cape-symbol) "cape" nil t)
  (autoload-if-found '(cape-keyword) "cape-keyword" nil t)

  (add-hook 'prog-mode-hook (lambda ()
			      (add-to-list 'completion-at-point-functions #'cape-file)
			      (add-to-list 'completion-at-point-functions #'cape-dabbrev)
			      (add-to-list 'completion-at-point-functions #'cape-keyword)
			      (add-to-list 'completion-at-point-functions #'cape-symbol))))

;; kind-icon
(minima
 :clone "jdtsmith/kind-icon")

(with-delayed-execution
  (add-to-list 'load-path (locate-user-emacs-file "el-clone/kind-icon"))

  (autoload-if-found '(kind-icon-margin-formatter corfu-default) "kind-icon" nil t)

  (with-eval-after-load 'corfu
    (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

  (setq kind-icon-default-face 'corfu-default))

;; vertico
(minima
 :clone "minad/vertico")

(with-delayed-execution-priority-high
  (add-to-list 'load-path (locate-user-emacs-file "el-clone/vertico"))

  (when (require 'vertico)
    (setq vertico-count 20)
    (vertico-mode)))

(minima
 :clone "radian-software/prescient.el")

(with-delayed-execution-priority-high
  (add-to-list 'load-path (locate-user-emacs-file "el-clone/prescient"))

  (autoload-if-found '(prescient-persist-mode) "prescient" nil t)
  (autoload-if-found '(corfu-prescient-mode) "corfu-prescient" nil t)
  (autoload-if-found '(vertico-prescient-mode) "vertico-prescient" nil t)

  (setq prescient-aggresive-file-save t)
  (setq prescient-save-file (locate-user-emacs-file "prescient-save.el"))

  (corfu-prescient-mode)
  (vertico-prescient-mode)
  (prescient-persist-mode))

;; consult
(minima
 :clone "minad/consult")

(with-delayed-execution-priority-high
  (add-to-list 'load-path (locate-user-emacs-file "el-clone/consult"))

  (autoload-if-found '(consult-customize consult-line) "consult" nil t)

  (define-key global-map (kbd "M-c") nil)
  (global-set-key (kbd "M-y") #'consult-yank-pop)
  (global-set-key (kbd "C-s") #'consult-line)
  (global-set-key (kbd "M-c s") #'isearch-forward)
  (global-set-key (kbd "M-s r") #'consult-ripgrep)
  
  (setq consult-preview-raw-size 1024000)
  (setq consult-preview-max-size 1024000)
  (consult-customize
   :preview-key (kbd "M-.")))

;; orderless
(minima
 :clone "oantolin/orderless")

(with-delayed-execution-priority-high
  (add-to-list 'load-path (locate-user-emacs-file "el-clone/orderless"))

  (when (require 'orderless)
    (setq completion-styles '(orderless basic))
    (setq completion-category-overrides '((file (styles basic partial-completion))))))

;; marginalia
(minima
 :clone "minad/marginalia")

(with-delayed-execution-priority-high
  (add-to-list 'load-path (locate-user-emacs-file "el-clone/marginalia"))

  (when (require 'marginalia)
    (marginalia-mode)))

;; affe
(minima
 :clone "minad/affe")

(with-delayed-execution-priority-high
  (add-to-list 'load-path (locate-user-emacs-file "el-clone/affe"))

  (autoload-if-found '(affe-grep affe-find) "affe" nil t)
  (autoload-if-found '(orderless-highlight-matches orderless-pattern-compiler) "orderless" nil t)

  (global-set-key (kbd "M-s a g") #'affe-grep)
  (global-set-key (kbd "M-s a f") #'affe-find)

  (setq affe-highlight-function #'orderless-highlight-matches)
  (setq affe-regexp-function #'orderless-pattern-compiler))

;; consult-ghq
(minima
 :clone "tomoya/consult-ghq")

(with-delayed-execution-priority-high
  (add-to-list 'load-path (locate-user-emacs-file "el-clone/consult-ghq"))

  (autoload-if-found '(consult-ghq-find) "consult-ghq" nil t)

  (global-set-key (kbd "C-c C-g") #'consult-ghq-find))

;; autorevert

(with-delayed-execution
  (global-auto-revert-mode 1))

;; savehist
(with-delayed-execution-priority-high
  (savehist-mode))

;; undohist
(minima
 :clone "emacsorphanage/undohist")

(with-delayed-execution
  (add-to-list 'load-path (locate-user-emacs-file "el-clone/undohist"))

  (when (require 'undohist)
    (undohist-initialize)))

;; vundo
(minima
 :clone "casouri/vundo")

(with-delayed-execution
  (add-to-list 'load-path (locate-user-emacs-file "el-clone/vundo"))

  (when (require 'vundo)
    (global-set-key (kbd "C-x u") #'vundo)
    (setq vundo-glyph-alist vundo-unicode-symbols)
    (set-face-attribute 'vundo-default nil :family "Symbola")))

;; smartparens
(minima
 :clone "Fuco1/smartparens")

(with-delayed-execution
  (add-to-list 'load-path (locate-user-emacs-file "el-clone/smartparens"))

  (autoload-if-found '(smartparens-mode) "smartparens-config" nil t)

  (add-hook 'prog-mode-hook #'smartparens-mode))

;; hl-line
(with-delayed-execution-priority-high
  (global-hl-line-mode))

;; volatile-highlights
(minima
 :clone "k-talo/volatile-highlights.el")

(with-delayed-execution
  (add-to-list 'load-path (locate-user-emacs-file "el-clone/volatile-highlights"))

  (autoload-if-found '(volatile-highlights-mode) "volatile-highlights" nil t)

  (custom-set-faces '(vhl/default-face ((nil (:foreground "#FF3333" :background "#FFCDCD")))))

  (volatile-highlights-mode t))

;; highlight-indent-guides
(minima
 :clone "eowiz/highlight-indent-guides")

(with-delayed-execution
  (add-to-list 'load-path (locate-user-emacs-file "el-clone/highlight-indent-guides"))

  (autoload-if-found '(highlight-indent-guides-mode) "highlight-indent-guides" nil t)

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

  (setq highlight-indent-guides-method 'bitmap)
  (setq highlight-indent-guides-bitmap-function #'my/highlight-indent-guides--bitmap-line)
  (setq highlight-indent-guides-auto-character-face-perc 40)
  (setq highlight-indent-guides-auto-top-character-face-perc 100)

  (setq highlight-indent-guides-auto-enabled t)
  (setq highlight-indent-guides-responsive t)

  (add-hook 'prog-mode-hook #'highlight-indent-guides-mode))

;; rainbow-delimiters
(minima
 :clone "Fanael/rainbow-delimiters")

(with-delayed-execution
  (add-to-list 'load-path (locate-user-emacs-file "el-clone/rainbow-delimiters"))

  (autoload-if-found '(rainbow-delimiters-mode) "rainbow-delimiters" nil t)

  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

;; eshell
(with-delayed-execution
  (require 'ansi-color)
  (require 'eshell)
  (defun eshell-handle-ansi-color ()
    (ansi-color-apply-on-region eshell-last-output-start
                                eshell-last-output-end))
  (add-to-list 'eshell-output-filter-functions 'eshell-handle-ansi-color))

;; git
(minima
 :clone "Artawower/blamer.el")

(with-delayed-execution
  (add-to-list 'load-path (locate-user-emacs-file "el-clone/blamer"))

  (autoload-if-found '(blamer-mode) "blamer" nil t))

(minima
 :clone "emacsmirror/git-timemachine")

(with-delayed-execution
  (add-to-list 'load-path (locate-user-emacs-file "el-clone/git-timemachine"))

  (autoload-if-found '(git-timemachine) "git-timemachine" nil t))

;; magit
(minima
 :clone "magit/with-editor")

(with-delayed-execution
  (add-to-list 'load-path (locate-user-emacs-file "el-clone/with-editor/lisp")))

(minima
 :clone "magit/magit")

(with-delayed-execution
  (add-to-list 'load-path (locate-user-emacs-file "el-clone/magit/lisp"))

  (autoload-if-found '(magit) "magit" nil t)

  (global-set-key (kbd "C-x g") #'magit))

;; git-modes
(minima
 :clone "magit/git-modes")

(with-delayed-execution
  (add-to-list 'load-path (locate-user-emacs-file "el-clone/git-modes"))

  (autoload-if-found '(gitattribute-mode gitconfig-mode gitignore-mode) "git-modes" nil t))

;; git-gutter
(minima
 :clone "emacsorphanage/git-gutter")

(with-delayed-execution
  (add-to-list 'load-path (locate-user-emacs-file "el-clone/git-gutter"))

  (when (require 'git-gutter)
    (global-git-gutter-mode +1)

    (setq git-gutter:unchaged-sign " ")
    (setq git-gutter:modified-sign " ")
    (setq git-gutter:added-sign " ")
    (setq git-gutter:deleted-sign " ")

    (set-face-background 'git-gutter:unchanged (face-attribute 'line-number :background))
    (set-face-background 'git-gutter:modified "#f1fa8c")
    (set-face-background 'git-gutter:added "#50fa7b")
    (set-face-background 'git-gutter:deleted "#ff79c6")))

;; hide-mode-line
(minima
 :clone "hlissner/emacs-hide-mode-line")

(with-delayed-execution
  (add-to-list 'load-path (locate-user-emacs-file "el-clone/emacs-hide-mode-line"))

  (add-hook 'neotree-mode-hook #'hide-mode-line-mode)
  (add-hook 'imenu-list-minor-mode-hook #'hide-mode-line-mode)
  (add-hook 'minimap-mode-hook #'hide-mode-line-mode))

;; flycheck
(minima
 :clone "flycheck/flycheck")

(with-delayed-execution
  (add-to-list 'load-path (locate-user-emacs-file "el-clone/flycheck"))

  (when (require 'flycheck)
    (global-flycheck-mode)))

;; flycheck-posframe
(minima
 :clone "alexmurray/flycheck-posframe")

;; (with-delayed-execution
;;   (add-to-list 'load-path (locate-user-emacs-file "el-clone/flycheck-posframe"))

;;   (autoload-if-found '(flycheck-posframe-mode flycheck-posframe-configure-pretty-defaults) "flycheck-posframe" nil t)

;;   (add-hook 'flycheck-mode-hook #'flycheck-posframe-mode)

;;   (flycheck-posframe-configure-pretty-defaults)
;;   (setq flycheck-posframe-border-use-error-face t)
;;   (setq flycheck-posframe-border-width 1)
;;   )

;; sideline
(minima
 :clone "emacs-sideline/sideline")

(with-delayed-execution-priority-high
  (setq sideline-backends-right '((sideline-flycheck . down)))

  (add-to-list 'load-path (locate-user-emacs-file "el-clone/sideline"))

  (autoload-if-found '(sideline-mode) "sideline" nil t)

  ;; (add-hook 'flycheck-mode-hook #'sideline-mode)
  )

;; sideline-flycheck
;; 横幅の計算が正しく行われないためコメントアウト
;; (minima
;;  :clone "emacs-sideline/sideline-flycheck")

;; (with-delayed-execution-priority-high
;;   (add-to-list 'load-path (locate-user-emacs-file "el-clone/sideline-flycheck"))

;;   (autoload-if-found '(sideline-flycheck-setup) "sideline-flycheck" nil t)

;;   (add-hook 'flycheck-mode-hook #'sideline-flycheck-setup))

;; treemacs
(minima
 :clone "Alexander-Miller/treemacs")

(with-delayed-execution-priority-high
  (add-to-list 'load-path (locate-user-emacs-file "el-clone/treemacs/src/elisp"))
  (add-to-list 'load-path (locate-user-emacs-file "el-clone/treemacs/src/extra")))

;; tree-sitter
(minima
 :clone "emacs-tree-sitter/elisp-tree-sitter")

(with-delayed-execution
  (add-to-list 'load-path (locate-user-emacs-file "el-clone/elisp-tree-sitter/core"))
  (add-to-list 'load-path (locate-user-emacs-file "el-clone/elisp-tree-sitter/lisp"))
  (add-to-list 'load-path (locate-user-emacs-file "el-clone/elisp-tree-sitter/langs"))

  (require 'tree-sitter)
  (require 'tree-sitter-hl)
  (require 'tree-sitter-debug)
  (require 'tree-sitter-query)
  (require 'tree-sitter-langs)

  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)

  (global-tree-sitter-mode))

;; treesit-auto
(minima
 :clone "renzmann/treesit-auto")

(with-delayed-execution
  (add-to-list 'load-path (locate-user-emacs-file "el-clone/treesit-auto"))

  (autoload-if-found '(make-treesit-auto-recipe global-treesit-auto-mode) "treesit-auto" nil t)

  ;; Some one make astro-ts-mode then uncomment
  ;; (setq astro-tsauto-config
  ;; 	(make-treesit-auto-recipe
  ;; 	 :lang 'astro
  ;; 	 :ts-mode 'astro-ts-mode
  ;; 	 :remap 'astro-mode
  ;; 	 :url "https://github.com/virchau13/tree-sitter-astro"
  ;; 	 :revision "master"
  ;; 	 :source-dir "src"))
  
  ;; (add-to-list 'treesit-auto-recipe-list astro-tsauto-config)

  (global-treesit-auto-mode))


;; ts-fold
(minima
 :clone "emacs-tree-sitter/ts-fold")

(with-delayed-execution
  (add-to-list 'load-path (locate-user-emacs-file "el-clone/ts-fold"))

  (autoload-if-found '(ts-fold-indicators-mode ts-fold-toggle) "ts-fold-indicators" nil t)

  (add-hook 'tree-sitter-after-on-hook #'ts-fold-indicators-mode)
  (add-hook 'ts-fold-mode-hook (lambda ()
                                 (local-set-key (kbd "C-<return>") #'ts-fold-toggle))))

;; fringe-helper
(minima
 :clone "nschum/fringe-helper.el")

;;
;; For Programming Lanaugages
;;

;; paredit
(minima
 :clone "emacsmirror/paredit")

(with-delayed-execution-priority-high
  (add-to-list 'load-path (locate-user-emacs-file "el-clone/paredit"))

  (autoload-if-found '(enable-paredit-mode) "paredit" nil t)

  (add-hook 'emacs-lisp-mode-hook #'enable-paredit-mode))

;; js-mode
(with-delayed-execution
  (setq js-indent-level 2)

  (with-eval-after-load 'lsp-mode
    (add-hook 'js-mode-hook #'lsp-deferred)))

;; typescript
(minima
 :clone "emacs-typescript/typescript.el")

(with-delayed-execution
  (add-to-list 'load-path (locate-user-emacs-file "el-clone/typescript"))

  (autoload-if-found '(typescript-ts-mode) "typescript" nil t)

  (add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . typescript-ts-mode))

  (setq typescript-indent-level 2)
  )

;; web-mode
(minima
 :clone "fxbois/web-mode")

(with-delayed-execution
  (add-to-list 'load-path (locate-user-emacs-file "el-clone/web-mode"))

  (autoload-if-found '(web-mode) "web-mode" nil t))

;; Astro
(with-delayed-execution
  (define-derived-mode astro-mode web-mode "astro")

  (add-hook 'astro-mode-hook
	    (lambda ()
	      (make-local-variable 'js-indent-level)
	      (setq js-indent-level 2)

	      (make-local-variable 'web-mode-markup-indent-offset)
	      (setq web-mode-markup-indent-offset 2)

	      (make-local-variable 'web-mode-code-indent-offset)
	      (setq web-mode-code-indent-offset 2)

	      (make-local-variable 'web-mode-enable-current-element-highlight)
	      (setq web-mode-enable-current-element-highlight t)))

  (setq auto-mode-alist (append '(("\\.astro\\'" . astro-mode))
				auto-mode-alist))
  
  (with-eval-after-load 'lsp-mode
    (add-to-list 'lsp-language-id-configuration
		 '(astro-mode . "astro"))

    (lsp-register-client
     (make-lsp-client :new-connection (lsp-stdio-connection '("astro-ls" "--stdio"))
		      :activation-fn (lsp-activate-on "astro")
		      :server-id 'astro-ls)))
  )

;; prettier
(minima
 :clone "prettier/prettier-emacs")

(with-delayed-execution
  (add-to-list 'load-path (locate-user-emacs-file "el-clone/prettier-emacs"))

  (autoload-if-found '(prettier-js-mode) "prettier-js" nil t)

  (add-hook 'js2-mode-hook 'prettier-js-mode)
  (add-hook 'web-mode 'prettier-js-mode)
  (add-hook 'astro-mode 'prettier-js-mode))

;; org-mode
(minima
 :clone "minad/org-modern")

(with-delayed-execution
  (add-to-list 'load-path (locate-user-emacs-file "el-clone/org-modern"))

  (autoload-if-found '(org-modern-mode org-modern-agenda) "org-modern" nil t)

  (add-hook 'org-mode-hook #'org-modern-mode)
  (add-hook 'org-agenda-finalize-hook #'org-modern-agenda)

  (dolist (face '(window-divider
                  window-divider-first-pixel
                  window-divider-last-pixel))
    (face-spec-reset-face face)
    (set-face-foreground face (face-attribute 'default :background)))
  (set-face-background 'fringe (face-attribute 'default :background))

  (setq org-log-done 'time)
  (setq org-auto-align-tags nil)
  (setq org-tags-column 0)
  (setq org-catch-invisible-edits 'show-and-error)
  (setq org-special-ctrl-a/e t)
  (setq org-insert-heading-respect-content t)
  (setq org-hide-emphasis-markers t)
  (setq org-pretty-entities t)
  (setq org-ellipsis "…")

  (setq org-agenda-tags-column 0)
  (setq org-agenda-block-separator ?─)
  (setq org-agenda-time-grid
	'((daily today require-timed)
	  (800 1000 1200 1400 1600 1800 2000)
	  " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄")
	org-agenda-current-time-string
	"⭠ now ─────────────────────────────────────────────────")
  )

(with-delayed-execution
  (setq org-src-tab-acts-natively t)
  (setq org-src-preserve-indentation t)
  (setq org-edit-src-content-indentation 0)
  (setq org-agenda-files '("~/org"))
  (setq org-src-fontify-natively t))

;; org-outer-indent
(minima
 :clone "rougier/org-outer-indent")

(with-delayed-execution
  (add-to-list 'load-path (locate-user-emacs-file "el-clone/org-outer-indent"))

  (autoload-if-found '(org-outer-indent-mode) "org-outer-indent" nil t))

;; Markdown
(minima
 :clone "polymode/polymode")

(with-delayed-execution
  (add-to-list 'load-path (locate-user-emacs-file "el-clone/polymode")))

(minima
 :clone "polymode/poly-markdown")

(minima
 :clone "jrblevin/markdown-mode")

(with-delayed-execution
  (add-to-list 'load-path (locate-user-emacs-file "el-clone/poly-markdown"))
  (add-to-list 'load-path (locate-user-emacs-file "el-clone/markdown-mode"))

  (autoload-if-found '(markdown-mode gfm-mode) "markdown-mode" nil t)
  (autoload-if-found '(poly-markdown-mode) "poly-markdown" nil t)

  (add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode))
  (add-to-list 'auto-mode-alist '("\\.md\\'" . poly-markdown-mode))

  (with-eval-after-load 'markdown-mode
    (setq tab-width 2)
    (setq indent-tabs-mode nil)
    (setq markdown-code-lang-modes (append '(("diff" . diff-mode))
					   markdown-code-lang-modes))))

;; lsp-mode
(setq lsp-use-plists t)
(minima
 :clone "emacs-lsp/lsp-mode")

(with-delayed-execution
  (add-to-list 'load-path (locate-user-emacs-file "el-clone/lsp-mode"))
  (add-to-list 'load-path (locate-user-emacs-file "el-clone/lsp-mode/clients"))

  (require 'lsp-mode)
  (require 'lsp-lens)
  (require 'lsp-modeline)
  (require 'lsp-headerline)
  (require 'lsp-completion)

  (setq read-process-output-max (* 1024 1024))
  (setq lsp-keymap-prefix "M-l")
  (setq lsp-idle-delay 0.5)
  (setq lsp-log-io nil)
  (setq lsp-completion-provider :none)

  (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration)

  ;; Turning
  (setq read-process-output-max (* 1024 1024))
  )

;; lsp-treemacs
(minima
 :clone "emacs-lsp/lsp-treemacs")

;; lsp-ui
(minima
 :clone "emacs-lsp/lsp-ui")

(with-delayed-execution-priority-high
  (add-to-list 'load-path (locate-user-emacs-file "el-clone/lsp-ui"))

  (with-eval-after-load 'lsp-mode
    (require 'lsp-ui)

    (setq lsp-ui-peek-always-show t)
    (setq lsp-ui-sideline-show-hover t)

    ;; lsp-ui-doc
    (setq lsp-ui-doc-enable t)
    (setq lsp-ui-doc-show-with-cursor t)
    (setq lsp-ui-doc-use-childframe t)
    (setq lsp-ui-doc-alignment 'window)
    (define-key lsp-ui-mode-map (kbd "s-l h f") #'lsp-ui-doc-focus-frame)

    ;; lsp-ui-imenu
    (setq lsp-ui-imenu-auto-refresh t)

    ;; lsp-ui-sideline
    (setq lsp-ui-sideline-enable t)
    (setq lsp-ui-sideline-show-symbol t)
    (setq lsp-ui-sideline-show-code-actions t)
    (setq lsp-ui-sideline-show-diagnostics t)
    (setq lsp-ui-sideline-delay 0.5)

    ;; lsp-ui-peek
    (setq lsp-ui-peek-enable t)
    (setq lsp-ui-peek-always-show t)
    (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
    (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references)

    (setq lsp-ui-flycheck-enable t)))

;; lsp-java
(minima
 :clone "emacs-lsp/lsp-java")

(with-delayed-execution-priority-high
  (add-to-list 'load-path (locate-user-emacs-file "el-clone/lsp-java"))

  (autoload-if-found '(lsp) "lsp-mode" nil t)

  (with-eval-after-load 'lsp-mode
    (require 'lsp-mode)
    (add-hook 'java-mode-hook #'lsp)))

;; yaml
(minima
 :clone "yoshiki/yaml-mode")

(with-delayed-execution
  (add-to-list 'load-path (locate-user-emacs-file "el-clone/yaml-mode"))

  (autoload-if-found '(yaml-mode) "yaml-mode" nil t)

  (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode)))

;; java

;; groovy
(minima
 :clone "Groovy-Emacs-Modes/groovy-emacs-modes"
 :priority 'low)

(with-delayed-execution
  (autoload-if-found '(groovy-mode) "groovy-emacs-modes" nil t)
  
  (add-to-list 'auto-mode-alist '("\\.groovy\\'" . groovy-mode)))

;; coq
(minima
 :clone "ProofGeneral/PG"
 :priority 'low)

(with-delayed-execution
  (add-to-list 'load-path (locate-user-emacs-file "el-clone/PG/generic"))
  (add-to-list 'load-path (locate-user-emacs-file "el-clone/PG/lib"))
  (add-to-list 'load-path (locate-user-emacs-file "el-clone/PG/coq"))

  (autoload-if-found '(coq-mode) "coq" nil t)

  (add-to-list 'auto-mode-alist '("\\.v\\'" . (lambda ()
						(load "~/.emacs.d/el-clone/PG/generic/proof-site")
						(coq-mode))))
  )

;;;###autoload
(defun open-init-org ()
  "Toggle current buffer between init.org."
  (interactive)
  (let ((path (buffer-file-name)))
    (if (equal path (expand-file-name "~/.emacs.d/inits/init.org"))
        (switch-to-buffer (other-buffer))
      (find-file "~/.emacs.d/init.el"))))

(with-delayed-execution-priority-high
  (global-set-key (kbd "M-SPC") #'open-init-org))

;;;###autoload
(defun my/native-comp-packages ()
  (interactive)
  (native-compile-async "~/.emacs.d/init.el")
  (native-compile-async "~/.emacs.d/early-init.el")
  (native-compile-async "~/.emacs.d/el-clone" 'recursively))

;; ;; org-mode

;; (leaf org
;;   :bind (("<f12>" . org-agenda))
;;   :setq-default ((org-enforce-todo-dependencies . t))
;;   :custom (
;;            (org-src-tab-acts-natively . t)
;;            (org-src-preserve-indentation . t)
;;            (org-edit-src-content-indentation . 0)
;;            ;; (org-agenda-files . '("~/org"))
;;            (org-src-fontify-natively . t)
;;            ;; (org-adapt-indentation . t)
;;            (org-edit-src-content-indentation . 0)
;;            )
;;   )

;; (defun consult-thing-at-point (&optional at-point)
;;   "Consult-line uses things-at-point."
;;   (interactive "P")
;;   (consult-line (thing-at-point 'symbol)))

;; (leaf embark
;;   :ensure t
;;   :bind (("C-." . embark-act)
;;          ("C-;" . embark-dwim)
;;          ("C-h B" . embark-bindings))
;;   :init
;;   (setq prefix-help-command #'embark-prefix-help-command)

;;   :config
;;   ;; Hide the mode line of the Embark live/completions buffers
;;   (add-to-list 'display-buffer-alist
;;                '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
;;                  nil
;;                  (window-parameters (mode-line-format . none)))))

;; (leaf embark-consult
;;   :ensure t
;;   :after embark consult
;;   :leaf-defer nil
;;   :hook ((embark-collect-mode . consult-preview-at-point-mode)))

;; ;;
;; ;; projectile
;; ;;

;; (leaf projectile
;;   :ensure t
;;   :diminish ""
;;   :config
;;   (projectile-mode +1))

;; (leaf consult-projectile
;;   :ensure t
;;   :bind (("C-c p f" . consult-projectile-find-file)))

;; (leaf imenu-list
;;   :ensure t
;;   :bind (("C-'" . imenu-list-smart-toggle)))

;; (leaf neotree
;;   :ensure t
;;   :bind (("M-\\" . neotree-projectile-toggle))
;;   :commands (neotree-show neotree-hide neotree-dir neotree-find)
;;   :custom ((neo-smart-open . t)
;;            (neo-show-hidden-files . t)
;;            (neo-create-file-auto-open . t)
;;            (neo-window-fixed-size . nil)
;;            (neo-window-width . 40))
;;   :config
;;   (setq neo-theme (if (display-graphic-p) 'icons 'arrow))

;;   (defun neotree-projectile-toggle ()
;;     (interactive)
;;     (let ((project-dir
;;            (ignore-errors
;;        ;;; Pick one: projectile or find-file-in-project
;;              (projectile-project-root)
;;              ))
;;           (file-name (buffer-file-name))
;;           (neo-smart-open t))
;;       (if (and (fboundp 'neo-global--window-exists-p)
;;                (neo-global--window-exists-p))
;;           (neotree-hide)
;;         (progn
;;           (neotree-show)
;;           (if project-dir
;;               (neotree-dir project-dir))
;;           (if file-name
;;               (neotree-find file-name)))))))

;; (leaf display-fill-column-indicator
;;   :hook git-commit-mode-hook
;;   :custom
;;   (display-fill-column-indicator-column . 50))

;; (leaf dumb-jump
;;   :ensure t
;;   :config

;;   (defhydra dumb-jump-hydra (:color blue :columns 3)
;;     "Dumb Jump"
;;     ("j" dumb-jump-go "Go")
;;     ("o" dumb-jump-go-other-window "Other window")
;;     ("e" dumb-jump-go-prefer-external "Go external")
;;     ("x" dumb-jump-go-prefer-external-other-window "Go external other window")
;;     ("i" dumb-jump-go-prompt "Prompt")
;;     ("l" dumb-jump-quick-look "Quick look")
;;     ("b" dumb-jump-back "Back")))

;; (leaf fsharp-mode
;;   :ensure t)

;; (leaf java
;;   :config
;;   (let* ((lombok-version "1.18.24")
;;          (lombok-jar-dir (expand-file-name (format "~/.m2/repository/org/projectlombok/lombok/%s/" lombok-version)))
;;          (lombok-jar-path (expand-file-name (format "~/.m2/repository/org/projectlombok/lombok/%s/lombok-%s.jar" lombok-version lombok-version)))
;;          (lombok-jar-url (format "https://repo1.maven.org/maven2/org/projectlombok/lombok/%s/lombok-%s.jar" lombok-version lombok-version)))
;;     (progn
;;       (if (not (file-exists-p lombok-jar-path))
;;           (progn
;;             (make-directory lombok-jar-dir t)
;;             (url-copy-file lombok-jar-url lombok-jar-path)))

;;       (leaf lsp-java
;;         :ensure t
;;         :custom ((lsp-java-format-enabled . t)
;;                  (lsp-java-vmargs . `("-Xmx1G"
;;                                       "-XX:+UseG1GC"
;;                                       "-XX:+UseStringDeduplication"
;;                                       ,(concat "-javaagent:" lombok-jar-path)
;;                                       ,(concat "-Xbootclasspath/a:" lombok-jar-path))))
;;         :hook ((java-mode-hook . lsp)
;;                (java-mode-hook . (lambda () (setq c-basic-offset 2))))))))

;; (leaf gradle-mode
;;   :ensure t)

;; (leaf groovy-mode
;;   :ensure t)

;; (leaf writeroom-mode
;;   :ensure t)

(setq file-name-handler-alist my/saved-file-name-handler-alist)

;;; init.el ends here

