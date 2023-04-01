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

(setq custom-file (locate-user-emacs-file "custom.el"))

;; Package Manager
(eval-when-compile
  (unless (file-directory-p (locate-user-emacs-file "elpa/el-clone"))
    (package-vc-install "https://github.com/takeokunn/el-clone.git")))

(eval-and-compile
  (add-to-list 'load-path (locate-user-emacs-file "elpa/el-clone"))
  (require 'el-clone))

;; NativeComp
(with-eval-after-load 'comp
  (setq native-comp-async-jobs-number 8)
  (setq native-comp-speed 3))

(eval-and-compile
  (setq byte-compile-warnings '(cl-functions))
  (require 'cl-lib nil t))

;;
;; packages
;;

;; compat
(eval-when-compile
  (el-clone :repo "emacs-compat/compat"))

(with-delayed-execution-priority-high
  (add-to-list 'load-path (locate-user-emacs-file "el-clone/compat")))

;; dash
(eval-when-compile
  (el-clone :repo "magnars/dash.el"))

(with-delayed-execution-priority-high
  (add-to-list 'load-path (locate-user-emacs-file "el-clone/dash")))

;; s
(eval-when-compile
  (el-clone :repo "magnars/s.el"))

(with-delayed-execution-priority-high
  (add-to-list 'load-path (locate-user-emacs-file "el-clone/s")))

;; f
(eval-when-compile
  (el-clone :repo "rejeep/f.el"))

(with-delayed-execution-priority-high
  (add-to-list 'load-path (locate-user-emacs-file "el-clone/f")))

;; ht
(eval-when-compile
  (el-clone :repo "Wilfred/ht.el"))

(with-delayed-execution-priority-high
  (add-to-list 'load-path (locate-user-emacs-file "el-clone/ht")))

;; pfuture
(eval-when-compile
 (el-clone :repo "Alexander-Miller/pfuture"))

(with-delayed-execution-priority-high
  (add-to-list 'load-path (locate-user-emacs-file "el-clone/pfuture")))

;; shrink-path
(eval-when-compile
  (el-clone :repo "zbelial/shrink-path.el"))

(with-delayed-execution-priority-high
  (add-to-list 'load-path (locate-user-emacs-file "el-clone/shrink-path")))

;; svg-lib
(eval-when-compile
  (el-clone :repo "rougier/svg-lib"))

(with-delayed-execution-priority-high
  (add-to-list 'load-path (locate-user-emacs-file "el-clone/svg-lib")))

;; request
(eval-when-compile
  (el-clone :repo "tkf/emacs-request"))

(with-delayed-execution-priority-high
  (add-to-list 'load-path (locate-user-emacs-file "el-clone/emacs-request")))

;; spinner
(eval-when-compile
  (el-clone :repo "Malabarba/spinner.el"))

(with-delayed-execution-priority-high
  (add-to-list 'load-path (locate-user-emacs-file "el-clone/spinner")))

;; hydra
(eval-when-compile
  (el-clone :repo "abo-abo/hydra"))

(with-delayed-execution-priority-high
  (add-to-list 'load-path (locate-user-emacs-file "el-clone/hydra")))

;; yasnippet
(eval-when-compile
  (el-clone :repo "joaotavora/yasnippet"))

(with-delayed-execution
  (add-to-list 'load-path (locate-user-emacs-file "el-clone/yasnippet")))

(with-delayed-execution
  (setq indent-tabs-mode nil
	make-backup-files nil
	auto-save-default nil
	suggest-key-bindings nil
	delete-by-moving-to-trash t)
  (setq insert-directory-program "gls")
  (defalias 'yes-or-no-p 'y-or-n-p))

;; exec-path-from-shell
(eval-when-compile
  (el-clone :repo "purcell/exec-path-from-shell"))

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
(eval-when-compile
  (el-clone :repo "Theory-of-Everything/everforest-emacs"))

(with-delayed-execution-priority-high
  (add-to-list 'load-path (locate-user-emacs-file "el-clone/everforest-emacs"))

  (when (require 'everforest-hard-dark-theme)
    (load-theme 'everforest-hard-dark t)))

;; all-the-icons
(eval-when-compile
  (el-clone :repo "domtronn/all-the-icons.el"))

(with-delayed-execution-priority-high
  (add-to-list 'load-path (locate-user-emacs-file "el-clone/all-the-icons")))

;; doom-modeline

(eval-when-compile
  (el-clone :repo "seagle0128/doom-modeline"))

(with-delayed-execution-priority-high
  (add-to-list 'load-path (locate-user-emacs-file "el-clone/doom-modeline"))

  (autoload-if-found '(doom-modeline-mode) "doom-modeline" nil t)

  (doom-modeline-mode 1))

(with-eval-after-load 'doom-modeline
  (setq doom-modeline-buffer-file-name-style 'truncate-with-project)
  (setq doom-modeline-icon t)
  (setq doom-modeline-major-mode-icon t)
  (setq doom-modeline-minor-modes nil))

;; scroll

(with-delayed-execution
  (pixel-scroll-precision-mode t)

  (setq scroll-margin 0)
  (setq scroll-conservatively 100000)
  (setq scroll-preserve-screen-position t))

;; ddskk

(eval-when-compile
  (el-clone :repo "skk-dev/ddskk"))

(with-delayed-execution-priority-high
  (add-to-list 'load-path (locate-user-emacs-file "el-clone/ddskk"))
  ;; (autoload-if-found '(skk-mode) "skk" nil t)

  (require 'skk-autoloads)

  (setq skk-preload t)
  (global-set-key (kbd "C-x C-j") #'skk-mode)
  (global-set-key (kbd "C-x ^") #'skk-kakutei)
  (add-hook 'text-mode-hook (lambda () (skk-mode) (skk-latin-mode-on)))
  (add-hook 'prog-mode-hook (lambda () (skk-mode) (skk-latin-mode-on))))

(eval-when-compile
  (el-clone :repo "conao3/ddskk-posframe.el"))

(with-delayed-execution-priority-high
  (add-to-list 'load-path (locate-user-emacs-file "el-clone/ddskk-posframe"))

  (autoload-if-found '(ddskk-posframe-mode) "ddskk-posframe" nil t)

  (with-eval-after-load 'skk
    (add-hook 'skk-mode-hook #'ddskk-posframe-mode)))

(eval-when-compile
  (el-clone :repo "tumashu/posframe"))

(with-delayed-execution-priority-high
  (add-to-list 'load-path (locate-user-emacs-file "el-clone/posframe")))

;; anzu
(eval-when-compile
  (el-clone :repo "emacsorphanage/anzu"))

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

;; corfu
(eval-when-compile
  (el-clone :repo "minad/corfu"))

(with-delayed-execution-priority-high
  (add-to-list 'load-path (locate-user-emacs-file "el-clone/corfu"))
  (add-to-list 'load-path (locate-user-emacs-file "el-clone/corfu/extensions"))

  (autoload-if-found '(corfu-popupinfo-mode) "corfu-popupinfo" nil t)

  (when (require 'corfu)
    (setq corfu-auto t)
    (setq corfu-cycle t)
    (setq tab-always-indent 'complete)
    (setq corfu-auto-prefix 2)
    (setq corfu-popupinfo-delay 0)

    (global-corfu-mode)

    (corfu-popupinfo-mode)))

;; cape
(eval-when-compile
  (el-clone :repo "minad/cape"))

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
(eval-when-compile
  (el-clone :repo "jdtsmith/kind-icon"))

(with-delayed-execution
  (add-to-list 'load-path (locate-user-emacs-file "el-clone/kind-icon"))

  (autoload-if-found '(kind-icon-margin-formatter corfu-default) "kind-icon" nil t)

  (with-eval-after-load 'corfu
    (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

  (setq kind-icon-default-face 'corfu-default))

;; vertico
(eval-when-compile
  (el-clone :repo "minad/vertico"))

(with-delayed-execution-priority-high
  (add-to-list 'load-path (locate-user-emacs-file "el-clone/vertico"))

  (when (require 'vertico)
    (setq vertico-count 20)
    (vertico-mode)))

(eval-when-compile
  (el-clone :repo "radian-software/prescient.el"))

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
(eval-when-compile
  (el-clone :repo "minad/consult"))

(with-delayed-execution-priority-high
  (add-to-list 'load-path (locate-user-emacs-file "el-clone/consult"))

  (autoload-if-found '(consult-customize consult-line) "consult" nil t)

  (global-set-key (kbd "M-y") #'consult-yank-pop)
  (global-set-key (kbd "C-s") #'consult-line)
  (global-set-key (kbd "M-s s") #'isearch-forward)
  (global-set-key (kbd "M-s r") #'consult-ripgrep)
  
  (setq consult-preview-raw-size 1024000)
  (consult-customize
   :preview-key "M-."))

;; orderless
(eval-when-compile
  (el-clone :repo "oantolin/orderless"))

(with-delayed-execution-priority-high
  (add-to-list 'load-path (locate-user-emacs-file "el-clone/orderless"))

  (when (require 'orderless)
    (setq completion-styles '(orderless basic))
    (setq completion-category-overrides '((file (styles basic partial-completion))))))

;; marginalia
(eval-when-compile
  (el-clone :repo "minad/marginalia"))

(with-delayed-execution-priority-high
  (add-to-list 'load-path (locate-user-emacs-file "el-clone/marginalia"))

  (when (require 'marginalia)
    (marginalia-mode)))

;; affe
(eval-when-compile
  (el-clone :repo "minad/affe"))

(with-delayed-execution-priority-high
  (add-to-list 'load-path (locate-user-emacs-file "el-clone/affe"))

  (autoload-if-found '(affe-grep affe-find) "affe" nil t)
  (autoload-if-found '(orderless-highlight-matches orderless-pattern-compiler) "orderless" nil t)

  (global-set-key (kbd "M-s a g") #'affe-grep)
  (global-set-key (kbd "M-s a f") #'affe-find)

  (setq affe-highlight-function #'orderless-highlight-matches)
  (setq affe-regexp-function #'orderless-pattern-compiler))

;; consult-ghq
(eval-when-compile
  (el-clone :repo "tomoya/consult-ghq"))

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
(eval-when-compile
  (el-clone :repo "emacsorphanage/undohist"))

(with-delayed-execution
  (add-to-list 'load-path (locate-user-emacs-file "el-clone/undohist"))

  (when (require 'undohist)
    (undohist-initialize)))

;; vundo
;; (eval-when-compile
;;   (el-clone :repo "casouri/vundo"))

;; (with-delayed-execution
;;   (add-to-list 'load-path (locate-user-emacs-file "el-clone/vundo"))

;;   (autoload-if-found '(vundo vundo-default) "vundo" nil t)

;;   (global-set-key (kbd "C-x u") #'vundo)

;;   (when (require 'vundo)
;;     (setq vundo-glyph-alist vundo-unicode-symbols)
;;     (set-face-attribute 'vundo-default nil :family "Symbola")))

;; smartparens
(eval-when-compile
  (el-clone :repo "Fuco1/smartparens"))

(with-delayed-execution
  (add-to-list 'load-path (locate-user-emacs-file "el-clone/smartparens"))

  (autoload-if-found '(smartparens-mode) "smartparens-config" nil t)

  (add-hook 'prog-mode-hook #'smartparens-mode))

;; volatile-highlights

(eval-when-compile
  (el-clone :repo "k-talo/volatile-highlights.el"))

(eval-when-compile
  (el-clone :repo "k-talo/volatile-highlights.el"))

(with-delayed-execution
  (add-to-list 'load-path (locate-user-emacs-file "el-clone/volatile-highlights"))

  (autoload-if-found '(volatile-highlights-mode) "volatile-highlights" nil t)

  (volatile-highlights-mode t))

;; git
(eval-when-compile
  (el-clone :repo "Artawower/blamer.el"))

(with-delayed-execution
  (add-to-list 'load-path (locate-user-emacs-file "el-clone/blamer"))

  (autoload-if-found '(blamer-mode) "blamer" nil t))

(eval-when-compile
  (el-clone :repo "emacsmirror/git-timemachine"))

(with-delayed-execution
  (add-to-list 'load-path (locate-user-emacs-file "el-clone/git-timemachine"))

  (autoload-if-found '(git-timemachine) "git-timemachine" nil t))

;; magit
(eval-when-compile
  (el-clone :repo "magit/magit"))

(with-delayed-execution
  (add-to-list 'load-path (locate-user-emacs-file "el-clone/magit/lisp"))

  (autoload-if-found '(magit) "magit" nil t)

  (global-set-key (kbd "C-x g") #'magit))

(eval-when-compile
  (el-clone :repo "magit/with-editor"))

(with-delayed-execution
  (add-to-list 'load-path (locate-user-emacs-file "el-clone/with-editor/lisp")))

;; git-modes
(eval-when-compile
  (el-clone :repo "magit/git-modes"))

(with-delayed-execution
  (add-to-list 'load-path (locate-user-emacs-file "el-clone/git-modes"))

  (autoload-if-found '(gitattribute-mode gitconfig-mode gitignore-mode) "git-modes" nil t))

;; git-gutter
(eval-when-compile
  (el-clone :repo "emacsorphanage/git-gutter"))

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
(eval-when-compile
  (el-clone :repo "hlissner/emacs-hide-mode-line"))

(with-delayed-execution
  (add-to-list 'load-path (locate-user-emacs-file "el-clone/emacs-hide-mode-line"))

  (add-hook 'neotree-mode-hook #'hide-mode-line-mode)
  (add-hook 'imenu-list-minor-mode-hook #'hide-mode-line-mode)
  (add-hook 'minimap-mode-hook #'hide-mode-line-mode))

;; flycheck
(eval-when-compile
  (el-clone :repo "flycheck/flycheck"))

(with-delayed-execution
  (add-to-list 'load-path (locate-user-emacs-file "el-clone/flycheck"))

  (when (require 'flycheck)
    (global-flycheck-mode)))

;; treemacs
(eval-when-compile
  (el-clone :repo "Alexander-Miller/treemacs"))

(with-delayed-execution-priority-high
  (add-to-list 'load-path (locate-user-emacs-file "el-clone/treemacs/src/elisp"))
  (add-to-list 'load-path (locate-user-emacs-file "el-clone/treemacs/src/extra")))

;; Markdown
(eval-when-compile
  (el-clone :repo "polymode/polymode"))

(with-delayed-execution
  (add-to-list 'load-path (locate-user-emacs-file "el-clone/polymode")))

(eval-when-compile
  (el-clone :repo "polymode/poly-markdown")
  (el-clone :repo "jrblevin/markdown-mode"))

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
(eval-when-compile
  (el-clone :repo "emacs-lsp/lsp-mode"))

(with-delayed-execution
  (add-to-list 'load-path (locate-user-emacs-file "el-clone/lsp-mode"))

  (require 'lsp-mode)
  (require 'lsp-lens)
  (require 'lsp-modeline)
  (require 'lsp-headerline)
  (require 'lsp-completion)

  (setq read-process-output-max (* 1024 1024))
  (setq lsp-keymap-prefix "M-l")
  (setq lsp-idle-delay 0.5)
  (setq lsp-log-io nil)
  (setq lsp-completion-provider :none))

;; lsp-treemacs
(eval-when-compile
  (el-clone :repo "emacs-lsp/lsp-treemacs"))

(with-delayed-execution-priority-high
  (add-to-list 'load-path (locate-user-emacs-file "el-clone/lsp-treemacs")))

;; lsp-ui
(eval-when-compile
  (el-clone :repo "emacs-lsp/lsp-ui"))

(with-delayed-execution-priority-high
  (add-to-list 'load-path (locate-user-emacs-file "el-clone/lsp-ui"))

  (with-eval-after-load 'lsp-mode
    (require 'lsp-ui)
    (setq lsp-ui-peek-always-show t)
    (setq lsp-ui-sideline-show-hover t)
    (setq lsp-ui-doc-enable t)))

;; lsp-java
(eval-when-compile
  (el-clone :repo "emacs-lsp/lsp-java"))

(with-delayed-execution-priority-high
  (add-to-list 'load-path (locate-user-emacs-file "el-clone/lsp-java"))

  (autoload-if-found '(lsp) "lsp-mode" nil t)

  (with-eval-after-load 'lsp-mode
    (require 'lsp-mode)
    (add-hook 'java-mode-hook #'lsp)))

;; yaml
(eval-when-compile
  (el-clone :repo "yoshiki/yaml-mode"))

(with-delayed-execution
  (add-to-list 'load-path (locate-user-emacs-file "el-clone/yaml-mode"))

  (autoload-if-found '(yaml-mode) "yaml-mode" nil t)

  (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode)))

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

(native-compile-async "~/.emacs.d/init.el")
(native-compile-async "~/.emacs.d/early-init.el")

(defun my/native-comp-packages ()
  (interactive)
  (native-compile-async "~/.emacs.d/init.el")
  (native-compile-async "~/.emacs.d/early-init.el")
  (native-compile-async "~/.emacs.d/el-clone" 'recursively))

;; (leaf mac
;;   :when (eq system-type 'darwin)
;;   :custom ((dired-use-ls-dired . t))
;;   :config
;;   (let ((gls (executable-find "gls")))
;;     (when gls
;;       (setq insert-directory-program gls
;;             dired-listing-switches "-aBhl --group-directories-first")))

;;   (leaf key-config
;;     :custom ((mac-command-modifier . 'alt)
;;              (mac-option-modifier . 'meta)
;;              (default-directory . "~/")
;;              (command-line-default-directory . "~/"))))

;; ;; バッファの一番上にある関数の名前を表示するためのパッケージ
;; (leaf topsy
;;   :el-get alphapapa/topsy.el
;;   :hook ((prog-mode-hook . topsy-mode)))

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

;; (leaf org-modern
;;   :ensure t
;;   :hook ((org-mode-hook . (lambda () (org-modern-mode 1)))
;;          (org-agenda-finalize-hook . #'org-modern-agenda))
;;   :config
;;   (dolist (face '(window-divider
;;                   window-divider-first-pixel
;;                   window-divider-last-pixel))
;;     (face-spec-reset-face face)
;;     (set-face-foreground face (face-attribute 'default :background)))
;;   (set-face-background 'fringe (face-attribute 'default :background))

;;   (setq
;;    ;; Edit settings
;;    org-auto-align-tags nil
;;    org-tags-column 0
;;    org-catch-invisible-edits 'show-and-error
;;    org-special-ctrl-a/e t
;;    org-insert-heading-respect-content t

;;    ;; Org styling, hide markup etc.
;;    org-hide-emphasis-markers t
;;    org-pretty-entities t
;;    org-ellipsis "…"

;;    ;; Agenda styling
;;    org-agenda-tags-column 0
;;    org-agenda-block-separator ?─
;;    org-agenda-time-grid
;;    '((daily today require-timed)
;;      (800 1000 1200 1400 1600 1800 2000)
;;      " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄")
;;    org-agenda-current-time-string
;;    "⭠ now ─────────────────────────────────────────────────"))

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
;; ;; Highlights
;; ;;

;; (leaf highlight-indent-guides
;;   :disabled t
;;   :diminish ""
;;   :ensure t
;;   :hook prog-mode-hook yaml-mode-hook
;;   :custom ((highlight-indent-guides-auto-enabled . t)
;;            (highlight-indent-guides-responsive . t)
;;            (highlight-indent-guides-method . 'character)))

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

;; (leaf tree-sitter
;;   :ensure t
;;   :hook ((tree-sitter-after-on-hook . tree-sitter-hl-mode))
;;   :global-minor-mode global-tree-sitter-mode

;;   :config
;;   (leaf tree-sitter-langs
;;     :ensure t
;;     :config
;;     (tree-sitter-require 'c)
;;     (tree-sitter-require 'rust)
;;     (tree-sitter-require 'typescript)
;;     (tree-sitter-require 'java)
;;     (tree-sitter-require 'elm)))

;; (leaf display-fill-column-indicator
;;   :hook git-commit-mode-hook
;;   :custom
;;   (display-fill-column-indicator-column . 50))

;; (leaf rainbow-mode
;;   :diminish ""
;;   :ensure t
;;   :hook prog-mode-hook)

;; (leaf lsp-mode
;;   :ensure t
;;   :pre-setq ((lsp-keymap-prefix . "M-l")
;;              (lsp-idle-delay . 0.5)
;;              (lsp-log-io . nil)
;;              (lsp-completion-provider . :none))
;;   :custom ((lsp-document-sync-method lsp--sync-incremental)))

;; (leaf lsp-ui
;;   :ensure t)

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

;; (leaf web-mode
;;   :ensure t
;;   :mode ("\\.html?\\'" "\\.njk\\'")
;;   :custom ((web-mode-markup-indent-offset . 2)))

;; (leaf js-mode
;;   :custom ((js-indent-level . 2)))

;; (leaf typescript-mode
;;   ;; :ensure t
;;   :el-get emacs-typescript/typescript.el
;;   :mode ("\\.ts?\\'" "\\.tsx?\\'")
;;   :custom ((indent-tabs-mode . nil)
;;            (typescript-indent-level . 2)))

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

