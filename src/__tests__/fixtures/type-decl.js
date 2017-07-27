declare module 'type-decl' {
  declare type stringOptions = { value: string }

  declare export function test(s: string, options: stringOptions): string
}
