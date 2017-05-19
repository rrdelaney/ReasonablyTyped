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

The main compiler is written is Reason.