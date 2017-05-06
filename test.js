declare module 'test-module' {
  declare type SquareOptions = {
    num: number,
    power: number
  }

  declare function square(x: number | SquareOptions): number
}