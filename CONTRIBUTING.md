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

# How is the code structured?

The main compiler is written is Reason, `compile` being defined in `src/retyped.re`. `src/retyped_node.re`
is compiled with JSOO and exposes `compile` to the JS target. `retyped_node.js` and `refmt_node.js` are
both interfaced by `index.js`. `index.js` exposes
a function for compiling called `compile` (different than the one exported from Reason) that
formats the code and handles errors cleanly. `cli.js` is the main entry for the `retyped` CLI tool, which
calls `index.js`.

The compiler is split into two parts, the module-definition generator and the code generator. The
module-definition generator is found in `src/modulegen.re` and the code generator is found in
`src/codegen.re`. The module-definition generator extracts an AST-like object from a Flow AST. This
is then passed to the code generator, which spits out Reason code as a string. The functionality from
both is glued together in `src/retyped.re`.

# Testing

To run tests:

```
$ npm test
```

Make sure you build the JS files first!

ReasonablyTyped uses AVA for testing. The main test suite can be found in `test`. Currently, the test
suite looks at all the `.js` files in `test/fixtures`, compiles them, and compares the result to a
corresponding `.re` file.
