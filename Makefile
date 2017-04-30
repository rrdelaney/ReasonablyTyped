INCLUDE=flow/src/parser
PKGS=sedlex
MAIN=src/index.native

all: build

build:
	rebuild \
		-Is $(INCLUDE) \
		-use-ocamlfind \
		-pkgs $(PKGS) \
		$(MAIN)