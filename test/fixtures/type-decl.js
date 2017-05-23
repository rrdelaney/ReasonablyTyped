declare module 'type-decl' {
  declare type StringOptions = { value: string }

  declare export function test (s: string, options: StringOptions): string
}
