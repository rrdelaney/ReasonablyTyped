declare module 'generics' {
  declare class Adder<X> {
    constructor<X>(x: X): Adder<X>;
    add(y: X): X;
  }

  declare interface SubOpts<M, N> {
    m: M;
    n: Adder<N>;
  }

  declare type Thing<X> = {
    lst: X[]
  }
}
