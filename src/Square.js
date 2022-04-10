import React from 'react';
import { colorToCss } from './Game';

class Square extends React.Component {

    constructor(props) {
        super(props);
        this.fila = 0;
        this.columna = 0;
    }

    // 
    render() {
        return (
            <div style={{ backgroundColor: colorToCss(this.props.value) }} />
        );
    }
}

export default Square;