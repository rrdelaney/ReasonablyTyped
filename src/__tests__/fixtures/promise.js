declare module 'promise' {
  declare export function pOfString(): Promise<string>
  declare export function pOfNumber(): Promise<number>
  declare export function pOfArray(): Promise<string[]>
  declare export function pOfVoid(): Promise<void>
  declare export function argPromise(p: Promise<string>): void
}
