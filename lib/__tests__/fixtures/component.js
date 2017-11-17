import type { Component } from 'react'

declare module 'react-component' {
  declare type props = { input?: string, b?: boolean }
  declare export class ReactComponent extends Component<props> {}
}
