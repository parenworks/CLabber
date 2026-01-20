# CLabber Makefile
# Builds a standalone executable using SBCL

SBCL := sbcl
TARGET := clabber
BUILD_SCRIPT := build.lisp

.PHONY: all clean run

all: $(TARGET)

$(TARGET): $(BUILD_SCRIPT) clabber.asd $(wildcard src/*.lisp) $(wildcard src/**/*.lisp)
	$(SBCL) --non-interactive --load $(BUILD_SCRIPT)

run: $(TARGET)
	./$(TARGET)

clean:
	rm -f $(TARGET)
	rm -rf ~/.cache/common-lisp/

# Development: run without building executable
dev:
	$(SBCL) --eval "(push #p\"$(shell pwd)/\" asdf:*central-registry*)" \
	        --eval "(ql:quickload :clabber)" \
	        --eval "(clabber:main)"
