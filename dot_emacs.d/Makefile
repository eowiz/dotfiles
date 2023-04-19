SHELL = /bin/zsh

EMACS ?= emacs

DDSKK_DIR                := el-clone/ddskk
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
	cd ~/.emacs.d/el-clone/PG && make

.PHONY: byte-compile-libraries
byte-compile-libraries:
	cp $(DDSKK_MAKEFILE_PATCH) $(DDSKK_DIR)
	cd $(DDSKK_DIR) && git apply $(DDSKK_MAKEFILE_PATCH) || :
	$(EMACS) -Q --batch --eval "(progn (add-to-list 'load-path (locate-user-emacs-file \"elpa/el-clone\")) (require 'el-clone) (el-clone-byte-compile))"
	make -C el-clone/ddskk install
	ln -s $(DDSKK_SITE_LISP_PATH) $(HOMEBREW_EMACS_SITE_LISP)

touch-highlight-indent-guides:
	touch el-clone/highlight-indent-guides/highlight-indent-guides.el

.PHONY: clean
clean: clean-init
	rm -rf elpa el-clone eln-cache init.eln init.elc early-init.eln early-init.elc
	rm -rf $(HOMEBREW_EMACS_SITE_LISP)

.PHONY: clean-init
clean-init:
	rm -rf init.elc early-init.elc
	rm -rf eln-cache/*/init-*.eln
