native:
	jbuilder build src/cli.exe

js:
	jbuilder build src/retyped_node.bc.js
	cp _build/default/src/retyped_node.bc.js lib/retyped_node.js 
