/**
 * @typechecks
 * @flow
 */

const React = require('React')
var { PropTypes } = React

class ReactNamePickerInlinedProps extends React.Component<{
  width: number,
  height: number
}> {
  render() {
    var wrapperTable = {
      height: this.props.height,
      width: this.props.width,
      overflow: 'hidden'
    }
    return <div>"Hello"</div>
  }
}
