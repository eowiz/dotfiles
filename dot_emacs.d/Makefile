SHELL = /bin/zsh

EMACS ?= emacs

DDSKK_DIR                := minima/ddskk
DDSKK_MAKEFILE_PATCH     := ddskk-Makefile.patch
DDSKK_SITE_LISP_PATH     := $(shell emacs -Q --batch --eval '(print (concat (expand-file-name (concat exec-directory "../../../../")) "share/emacs/site-lisp/skk"))')
HOMEBREW_EMACS_SITE_LISP := /opt/homebrew/share/emacs/site-lisp/skk

.PHONY: all
all: byte-compile make-ProofGeneral byte-compile-libraries touch-highlight-indent-guides

.PHONY: byte-compile
byte-compile:
	$(EMACS) -Q --batch -f batch-byte-compile early-init.el
	$(EMACS) -Q --batch -f batch-byte-compile init.el

make-ProofGeneral:
	cd ~/.emacs.d/minima/PG && make

.PHONY: byte-compile-libraries
byte-compile-libraries:
	cp $(DDSKK_MAKEFILE_PATCH) $(DDSKK_DIR)
	cd $(DDSKK_DIR) && git apply $(DDSKK_MAKEFILE_PATCH) || :
	$(EMACS) -Q --batch --eval "(progn (add-to-list 'load-path (locate-user-emacs-file \"local-packages/minima\")) (require 'minima) (minima-byte-compile))"
	make -C $(DDSKK_DIR) install
	ln -s $(DDSKK_SITE_LISP_PATH) $(HOMEBREW_EMACS_SITE_LISP)

touch-highlight-indent-guides:
	touch minima/highlight-indent-guides/highlight-indent-guides.el

.PHONY: clean
clean: clean-init
	rm -rf elpa minima eln-cache init.elc early-init.elc || true
	rm -rf $(HOMEBREW_EMACS_SITE_LISP) || true

.PHONY: clean-init
clean-init:
	rm -rf init.elc early-init.elc || true
	rm -rf eln-cache/*/init-*.eln || true
