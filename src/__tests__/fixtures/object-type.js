declare module 'object-type' {
  declare type subscribeOptions = {
    start: number,
    stop: number
  }

  declare type testOptions = {
    method: string,
    subscribe: subscribeOptions
  }

  declare export function test(options: TestOptions): string
}
