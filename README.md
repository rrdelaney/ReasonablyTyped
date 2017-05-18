# ReasonablyTyped

> Converts Flow library definitions to Bucklescript interfaces

# Installation

```
$ npm install --global reasonably-typed
```

# Usage

```
retyped

Commands:
  compile [files...]  Generate BuckleScript interfaces from a file

Options:
  --version     Show version number                                    [boolean]
  --no-fmt      Don't run the resulting code through refmt             [boolean]
  --help        Show help                                              [boolean]
```

This will generate a Bucklescript interface file for every library definition given. For example,
to generate interfaces from flow-typed, run:

```
$ retyped compile --flow-typed
```

For a single file, run:

```
$ retyped compile my-flow-lib.js
```

See the example directory for usage details.

For more information, run with `--help`.

# Usage as a library

ReasonablyTyped also exports a library for use! See the example below:

```js
// lib-usage.js
import * as ReasonablyTyped from 'reasonably-typed'

const libName = 'lib.js'
const libSrc = fs.readFileSync(libName).toString()
const bsInterface = ReasonablyTyped.compile(libName, libSrc)
```

### `compile (fileName: string, fileSource: string) => string`

Compiles a given Flow library definition to a Bucklescript interface.
This function only cares about _transforming_ the source, and doesn't write anything.

### `refmt (code: string, inFile: 'RE' | 'ML', fileType: 'interface' | 'implementation', outFile: 'RE' | 'ML') => string`

Formats a block of code using `refmt`

### `compileSource (code: string) => string`

Compiles a libdef, formats the result, and handles errors cleanly

# Roadmap

- [x] Basic types like `string`
- [x] Function types
- [x] Record types
- [ ] Literals as types
- [x] Union types
- [ ] Instersection types
- [x] Named types
- [ ] Classes
- [ ] Generics
- [ ] Built-ins like Promises and React

# Contributing

Right now the areas that need the most help are:
- Union types
- Generics

# Building

There's a Makefile for the project, so compilation _should_ be easy. There's two targets right now,
native and JS. Native is mostly used for testing and not distributed any where. The JS target is what
is distributed on NPM and is interfaced by `yargs`.

To build for native:

```
$ make
```

To build JS:

```
$ make js
```