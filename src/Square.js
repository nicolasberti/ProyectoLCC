import React from 'react';
import { __SECRET_INTERNALS_DO_NOT_USE_OR_YOU_WILL_BE_FIRED } from 'react-dom';
import { colorToCss } from './Game';

class Square extends React.Component {

    render() {
        if(this.props.origen === false){
            return ( // estilo para las celdas que no son origen
                <div 
                onClick={this.props.onClick}
                style={{ backgroundColor: colorToCss(this.props.value),
                    border: '1px solid #606060',
                    borderRadius: '5px'

                }} />
            );
        } else {
            return ( // estilo para la celda origen
                <div  
                onClick={this.props.onClick}
                style={{ backgroundColor: colorToCss(this.props.value),
                    border: '3px solid #000000', 
                    borderRadius: '5px'
                }}></div>
            );
        }
    }
}

export default Square;