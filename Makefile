# CLabber Makefile
# Builds a standalone executable using SBCL

SBCL := sbcl
TARGET := clabber
BUILD_SCRIPT := build.lisp
PREFIX := /usr/local
BINDIR := $(PREFIX)/bin

.PHONY: all build clean run install uninstall test dev

all: build

build: $(TARGET)

$(TARGET): $(BUILD_SCRIPT) clabber.asd $(wildcard src/*.lisp) $(wildcard src/**/*.lisp)
	$(SBCL) --non-interactive --load $(BUILD_SCRIPT)

run: $(TARGET)
	./$(TARGET)

install: $(TARGET)
	install -d $(DESTDIR)$(BINDIR)
	install -m 755 $(TARGET) $(DESTDIR)$(BINDIR)/$(TARGET)

uninstall:
	rm -f $(DESTDIR)$(BINDIR)/$(TARGET)

clean:
	rm -f $(TARGET)
	rm -rf ~/.cache/common-lisp/

# Development: run without building executable
dev:
	$(SBCL) --eval "(push #p\"$(shell pwd)/\" asdf:*central-registry*)" \
	        --eval "(ql:quickload :clabber)" \
	        --eval "(clabber:main)"

# Run OMEMO tests
test:
	$(SBCL) --non-interactive \
	        --eval "(push #p\"$(shell pwd)/\" asdf:*central-registry*)" \
	        --eval "(ql:quickload :clabber)" \
	        --load tests/omemo-tests.lisp \
	        --eval "(if (clabber::run-omemo-tests) (sb-ext:exit :code 0) (sb-ext:exit :code 1))"
