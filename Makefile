OCAMLC=rebuild
INCLUDE=flow/src/parser
PKGS=sedlex
MAIN=cli.native
JSC=js_of_ocaml
JS_MAIN=retyped_node.byte

all: refmt js

native:
	$(OCAMLC) \
		-cflags -bin-annot \
		-Is $(INCLUDE) \
		-use-ocamlfind \
		-pkgs $(PKGS) \
		src/$(MAIN)

byte:
	$(OCAMLC) \
		-cflags -bin-annot \
		-Is $(INCLUDE) \
		-use-ocamlfind \
		-pkgs $(PKGS) \
		-package js_of_ocaml \
		src/$(JS_MAIN)

js: byte
	$(JSC) \
		--pretty \
		--no-inline \
		--disable shortvar \
		--disable share \
		--enable excwrap \
		$(JS_MAIN)

	mv retyped_node.js lib

refmt:
	cd reason-tools && sh shell.sh
	mv reason-tools/_build/refmt/app.js lib/refmt_node.js

clean:
	rm -rf _build *.native *.byte
