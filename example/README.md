# ReasonablyTyped Example

This repo shows off example usage of ReasonablyTyped and the `retyped` command.

Here is an example of a Bucklescript project written in Reason using the
`is-absolute-url` library pulled from npm. Luckily, there is already a Flow
type definition for the library in `flow-typed`, so we can install it using
`flow-typed install`. This gives us the JS files found in the `flow-typed/`
directory. Using ReasonablyTyped, we can automatically generate an interface
for Bucklescript to use during compilation from the Flow library definition!
To do that, run `retyped flow-typed/npm/is-absolute-url_v2.x.x.js`. This generates
a file `flow-typed/npm/is_absolute_url.re` that Bucklescript can use. To include
the interfaces automatically, add `flow-typed/npm` to the sources field in your
Bucklescript config.