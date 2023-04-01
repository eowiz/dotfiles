
EMACS ?= emacs

DDSKK_DIR            = el-clone/ddskk
DDSKK_MAKEFILE_PATCH = ddskk-Makefile.patch

.PHONY: all
all: byte-compile byte-compile-libraries

.PHONY: byte-compile
byte-compile:
	$(EMACS) -Q --batch -f batch-byte-compile early-init.el
	$(EMACS) -Q --batch -f batch-byte-compile init.el

.PHONY: byte-compile-libraries
byte-compile-libraries:
	cp $(DDSKK_MAKEFILE_PATCH) $(DDSKK_DIR)
	cd $(DDSKK_DIR) && git apply $(DDSKK_MAKEFILE_PATCH) || :
	$(EMACS) -Q --batch --eval "(progn (add-to-list 'load-path (locate-user-emacs-file \"elpa/el-clone\")) (require 'el-clone) (el-clone-byte-compile))"
	make -C el-clone/ddskk elc

.PHONY: clean
clean:
	rm -rf elpa el-clone eln-cache init.eln init.elc early-init.eln early-init.elc
