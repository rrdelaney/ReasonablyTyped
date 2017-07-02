<h1 align="center"><a href="https://rrdelaney.github.io/ReasonablyTyped/">ReasonablyTyped</a></h1>

<pre align="center">
  $ npm install --global reasonably-typed
</pre>

<h4 align="center"><i>Converts Flow library definitions to Bucklescript interfaces</i></h4>

<hr>

### Take your Flow definition

```js
// class.js
declare module 'classes' {
  declare type State = {
    id: number,
    storeName: string
  };

  declare export class Store {
    constructor(initialState: State): Store;
    state: State;
    update(nextState: State): void;
  }
}
```

### Run `retyped`

<pre align="center">
$ retyped compile class.js
</pre>

### Get Reason

```reason
/* Module classes */

type state = Js.t {. id : float, storeName : string};

type store = Js.t {. state : state, update : nextState::state => unit [@bs.meth]};

external create_store : initialState::state => store =
  "Store" [@@bs.new] [@@bs.module "classes"];
```

<hr>

<p><details>
<summary><b>Usage as a library</b></summary>
ReasonablyTyped also exports a library for use! See the example below:

```js
// lib-usage.js
import * as ReasonablyTyped from 'reasonably-typed'

const libSrc = fs.readFileSync('lib.js').toString()
const bsInterface = ReasonablyTyped.compile(libSrc)
```
  
**`format (code: string) => string`**

Formats a block of code using `refmt`

**`compile (code: string, filename?: string) => string`**

Compiles a libdef, formats the result, and handles errors cleanly
</details></p>

<p><details>
<summary><b>Roadmap</b></summary>
- [x] Basic types like `string`
- [x] Function types
- [x] Record types
- [ ] Literals as types
- [x] Union types
- [ ] Instersection types
- [x] Named types
- [x] Optional parameters
- [x] Classes
- [ ] Generics
- [ ] Built-ins like Promises and React
</details></p>
