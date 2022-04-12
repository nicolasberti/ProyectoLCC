import React from 'react';
import { colorToCss } from './Game';

class Square extends React.Component {

    render() {
        return (
            <div 
            onClick={this.props.onClick}
            style={{ backgroundColor: colorToCss(this.props.value) }} />
        );
    }
}

export default Square;