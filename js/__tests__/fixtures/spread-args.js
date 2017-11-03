declare module 'spread-args' {
  declare export function foo(...bars: number[]): void
  declare export function optFoo(...bars?: number[]): void
  declare export function bothFoo(anArg: string, ...bars: number[]): void
  declare export function bothOptFoo(anArg: string, ...bars?: number[]): void
  declare export function soManyOpts(anArg?: string, ...bars?: number[]): void
  declare export function returns(anArg: string, ...bars: number[]): number
  declare export function multipleLists(foos: number[], ...bars: number[]): void
}
