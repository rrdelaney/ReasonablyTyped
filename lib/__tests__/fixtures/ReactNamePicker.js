/**
 * @typechecks
 * @flow
 */

const React = require('React')
var { PropTypes } = React

type myProps = { width: number, height: number }

class ReactNamePicker extends React.Component<myProps> {
  render() {
    var wrapperTable = {
      height: this.props.height,
      width: this.props.width,
      overflow: 'hidden'
    }
    return <div>"Hello"</div>
  }
}
