# Development Guide

## Introduction

ReasonablyTyped is a project that converts [Flow](https://flow.org) and [TypeScript](http://www.typescriptlang.org/)
type definitions to usable [ReasonML](https://reasonml.github.io) and [BuckleScript](https://bucklescript.github.io)
code. It's written primarally in ReasonML itself and compiled with BuckleScript. There's also some small JavaScript wrappers
for the command line and the npm library.

It works by parsing a [Flow](https://github.com/rrdelaney/ReasonablyTyped/blob/master/src/flowBsType.re) or
[TypeScript](https://github.com/rrdelaney/ReasonablyTyped/blob/master/src/typescriptBsType.re) AST, converting it into
[an intermediate representation](https://github.com/rrdelaney/ReasonablyTyped/blob/master/src/bsTypeAst.re), and then
printing it as [Reason code](https://github.com/rrdelaney/ReasonablyTyped/blob/master/src/bsTypeReason.re). There's also
a helpful little [Flow printer](https://github.com/rrdelaney/ReasonablyTyped/blob/master/src/bsTypeFlow.re) for debugging!

## Setting Up

First clone the repo from GitHub, however you see fit. Then make sure you have [Node.js](https://nodejs.org)
and [Yarn](https://yarnpkg.com/) nstalled on your system, you'll need them for development. You'll also
probably want to setup your editor for Reason development, so checkout
[this guide](https://reasonml.github.io/docs/en/global-installation.html).

Once all that stuff is installed, run:

```sh
$ yarn install
```

That should install BuckleScript for the project, and all the dependencies ReasonablyTyped needs!

## Building

Building the project is fairly easy, thanks to BuckleScript! Just run:

```sh
# Once-off builds
$ yarn bsb -clean-world -make-world

# Watch mode
$ yarn bsb -make-world -w
```

Then you can use the [command-line interface](https://github.com/rrdelaney/ReasonablyTyped/blob/master/lib/cli.js) to
test compile some code:

```sh
$ node lib/cli.js ./_test.js --debug
```

Make sure you have that `--debug` flag at the end! It prints out a ton of useful debugging information and makes sure
not to write any files.

## Testing

[Unit and integration testing](https://github.com/rrdelaney/ReasonablyTyped/tree/master/lib/__tests__) are done with
[Jest](https://facebook.github.io/jest). To run the tests:

```sh
$ yarn test
```
