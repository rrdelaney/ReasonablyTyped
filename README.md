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
  --help        Show help
```

This will generate a Bucklescript interface file for every library definition given. For example,
to generate interfaces from flow-typed, run:

```
$ retyped compile flow-typed/npm/*.js
```

See the example directory for usage details.

For more information, run with `-help`.

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
- Compiling to JS
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