OCAMLC=rebuild
INCLUDE=flow/src/parser
PKGS=sedlex
MAIN=cmd.native
JSC=js_of_ocaml
JS_MAIN=retyped.byte

all: native

native:
	$(OCAMLC) \
		-Is $(INCLUDE) \
		-use-ocamlfind \
		-package $(PKGS) \
		src/$(MAIN)

byte:
	$(OCAMLC) \
		-Is $(INCLUDE) \
		-use-ocamlfind \
		-package $(PKGS) \
		-package js_of_ocaml \
		src/$(JS_MAIN)

js: byte
	$(JSC) \
		$(JS_MAIN)