# ReasonablyTyped

> Converts Flow library definitions to Bucklescript interfaces

# Installation

```
$ npm install --global reasonably-typed
```

_Note: The JS build is currently outdated. Please use a native build for now_.

# Usage

```
$ retyped <...lib_defs>
```

This will generate a Bucklescript interface file for every library definition given. For example,
to generate interfaces from flow-typed, run:

```
$ retyped flow-typed/npm/*.js
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

## Native

```
$ make
```

## JS

```
$ make js
```