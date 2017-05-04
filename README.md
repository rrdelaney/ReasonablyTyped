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
- [ ] Union and intersection types
- [ ] Classes
- [ ] Generics
- [ ] Built-ins like Promises and React

# Building

## Native

```
$ make
```

## JS

```
$ make js
```