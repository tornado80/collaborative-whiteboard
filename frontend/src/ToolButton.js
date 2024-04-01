import './ToolButton.css'
import * as React from "react"
import Icon from "@mdi/react"

export default class ToolButton extends React.Component {
  onClick() {
    const fn = this.props.onClick
    if (fn) {
        fn(this.props.value)
    }
  }

  render() {
    return (
          <button
            className={"tool-button" + (this.props.selected ? " selected" : "")}
            onClick={ this.onClick.bind(this) }
            disabled={this.props.disabled}
          >
            {this.props.disabled}
            <Icon
              path={this.props.icon}
              size={2}
            />
          </button>
    );
  }
}
