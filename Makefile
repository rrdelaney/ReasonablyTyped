OCAMLC=rebuild
INCLUDE=flow/src/parser
PKGS=sedlex
MAIN=src/cmd.native

all: build

build:
	$(OCAMLC) \
		-Is $(INCLUDE) \
		-use-ocamlfind \
		-pkgs $(PKGS) \
		$(MAIN)