OCAMLC=rebuild
INCLUDE=flow/src/parser
PKGS=sedlex
MAIN=cmd.native
JSC=js_of_ocaml
JS_MAIN=cmd.byte

all: native

native:
	$(OCAMLC) \
		-Is $(INCLUDE) \
		-use-ocamlfind \
		-pkgs $(PKGS) \
		src/$(MAIN)

byte:
	$(OCAMLC) \
		-Is $(INCLUDE) \
		-use-ocamlfind \
		-pkgs $(PKGS) \
		src/$(JS_MAIN)

js: byte
	$(JSC) \
		$(JS_MAIN) \
		--custom-header="#!/usr/bin/env node"