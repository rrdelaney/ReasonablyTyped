declare module "multiple-modules" {
  declare export function someTopLevel(): void
}

declare module 'multiple-modules/first' {
  declare export function first(): number 
  declare export function second(): void
}

declare module 'multiple-modules/second' {
  declare export function second(): string 
  declare export function third(): void
}

declare module 'multiple-modules/third' {
  declare export function third(): string 
  declare export function fourth(): void
}

declare module 'multiple-modules/third/inner' {
  declare export function third(): string 
  declare export function fourth(): void
}