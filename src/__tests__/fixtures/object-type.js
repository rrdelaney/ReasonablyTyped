declare module 'object-type' {
  declare type SubscribeOptions = {
    start: number,
    stop: number
  }

  declare type TestOptions = {
    method: string,
    subscribe: SubscribeOptions
  }

  declare export function test(options: TestOptions): string
}
