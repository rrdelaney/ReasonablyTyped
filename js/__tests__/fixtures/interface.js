declare module 'interface' {
  declare interface Options {
    op: string;
  }

  declare function apply(x: number, y: number, options?: Options): number
}
