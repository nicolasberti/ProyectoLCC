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
                        key={i + "." + j} />
                    )
                )}
            </div>
        );
    }
}

export default Board;