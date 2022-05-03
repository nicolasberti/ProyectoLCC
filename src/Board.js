import React from 'react';
import Square from './Square';

class Board extends React.Component {
    
    render() {
        return (
            <div className="board">
                {this.props.grid.map((row, i) =>
                    row.map((cell, j) =>
                        <Square 
                        onClick={() => this.props.onClick(cell,i,j)}
                        value={cell}
                        key={i + "." + j} 
                        origen={this.props.origenFila===(i+1) && this.props.origenColumna===(j+1)}/> // se evalua si es la celda origen para diferenciarla de las demás
                    )
                )}
            </div>
        );
    }
}

export default Board;