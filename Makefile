native:
	jbuilder build src/cli.exe

js:
	jbuilder build src/retyped_node.bc.js
	cp _build/default/src/retyped_node.bc.js lib/retyped_node.js

test: js
	npm test

test-flow: js
	npm run test:flow-typed

test-typescript: js
	npm run test:definitely-typed
