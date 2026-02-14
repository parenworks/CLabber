# CLabber Makefile
# Builds a standalone executable using SBCL + C helper library

SBCL := sbcl
CC := gcc
TARGET := clabber
BUILD_SCRIPT := build.lisp
PREFIX := /usr/local
BINDIR := $(PREFIX)/bin

# C helper library for Signal Protocol FFI
SIGNAL_HELPER := libclabber-signal.so
SIGNAL_SRC := src/crypto/clabber-signal-helper.c
SIGNAL_CFLAGS := -shared -fPIC -Wall -Wextra -O2 -I/usr/include/signal
SIGNAL_LIBS := -lsignal-protocol-c -lssl -lcrypto

.PHONY: all build clean run install uninstall dev repl help lib

all: lib build

# Build the C helper library
lib: $(SIGNAL_HELPER)

$(SIGNAL_HELPER): $(SIGNAL_SRC)
	$(CC) $(SIGNAL_CFLAGS) -o $@ $< $(SIGNAL_LIBS)

# Build the Lisp executable
build: lib $(TARGET)

$(TARGET): $(BUILD_SCRIPT) clabber.asd $(wildcard src/*.lisp) $(wildcard src/**/*.lisp) $(SIGNAL_HELPER)
	$(SBCL) --non-interactive --load $(BUILD_SCRIPT)

run: $(TARGET)
	LD_LIBRARY_PATH=. ./$(TARGET)

install: $(TARGET) $(SIGNAL_HELPER)
	install -d $(DESTDIR)$(BINDIR)
	install -m 755 $(TARGET) $(DESTDIR)$(BINDIR)/$(TARGET)
	install -d $(DESTDIR)/usr/local/lib
	install -m 644 $(SIGNAL_HELPER) $(DESTDIR)/usr/local/lib/

uninstall:
	rm -f $(DESTDIR)$(BINDIR)/$(TARGET)
	rm -f $(DESTDIR)/usr/local/lib/$(SIGNAL_HELPER)

clean:
	rm -f $(TARGET) $(SIGNAL_HELPER)
	rm -rf ~/.cache/common-lisp/sbcl-*/**/clabber/

# Development: run from source
dev: lib
	find ~/.cache/common-lisp/ -path "*clabber*" -delete 2>/dev/null; true
	LD_LIBRARY_PATH=. $(SBCL) \
		--eval "(require :asdf)" \
		--eval "(load \"~/quicklisp/setup.lisp\")" \
		--eval "(push #p\"$(shell pwd)/\" asdf:*central-registry*)" \
		--eval "(asdf:clear-system :clabber)" \
		--eval "(ql:quickload :clabber)" \
		--eval "(clabber:main)"

# Load into REPL
repl: lib
	LD_LIBRARY_PATH=. $(SBCL) \
		--eval "(require :asdf)" \
		--eval "(load \"~/quicklisp/setup.lisp\")" \
		--eval "(push #p\"$(shell pwd)/\" asdf:*central-registry*)" \
		--eval "(ql:quickload :clabber)"

# Check syntax without running
check: lib
	LD_LIBRARY_PATH=. $(SBCL) --non-interactive \
		--eval "(require :asdf)" \
		--eval "(load \"~/quicklisp/setup.lisp\")" \
		--eval "(push #p\"$(shell pwd)/\" asdf:*central-registry*)" \
		--eval "(ql:quickload :clabber)" \
		--eval "(sb-ext:exit)"

help:
	@echo "CLabber - XMPP Chat Client with OMEMO"
	@echo ""
	@echo "Targets:"
	@echo "  all      - Build C helper library + executable"
	@echo "  lib      - Build C helper library only"
	@echo "  build    - Build standalone executable"
	@echo "  run      - Run the built executable"
	@echo "  dev      - Run from source (requires Quicklisp)"
	@echo "  repl     - Start SBCL with CLabber loaded"
	@echo "  check    - Check if code compiles"
	@echo "  install  - Install to $(BINDIR)"
	@echo "  clean    - Remove build artifacts"
	@echo "  help     - Show this help"
