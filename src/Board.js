import React from 'react';
import Square from './Square';

class Board extends React.Component {
    
    render() {
        return (
            <div className="board">
                {this.props.grid.map((row, i) =>
                    row.map((cell, j) =>
                        <Square value={cell}  fila = {i} columna = {j} key={i + "." + j} />
                    )
                )}
            </div>
        );
    }
}

export default Board;