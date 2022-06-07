import React from 'react';
import PengineClient from './PengineClient';
import Board from './Board';

/**
 * List of colors.
 */

const colors = ["r", "v", "p", "g", "b", "y"];  // red, violet, pink, green, blue, yellow

/**
 * Returns the CSS representation of the received color.
 */

export function colorToCss(color) {
  switch (color) {
    case "r": return "#FF6666";
    case "v": return "#B266FF";
    case "p": return "#FFCCE5";
    case "g": return "#99FF99";
    case "b": return "#6666FF";
    case "y": return "#FFFF99";
  }
  return color;
}


class Game extends React.Component {

  pengine;

  constructor(props) {
    super(props);
    this.state = {
      turns: 0,
      adyacentes: 0, // Almacena la cantidad de adyacentes a la celda origen.
      origenColor: null, // Color de la celda origen
      origenFila: 1, // Posicion i de la celda origen
      origenColumna: 1, // Posicion j de la celda origen
      grid: null,
      jugadas: "", // String para el historial de jugadas
      complete: false,  // true if game is complete, false otherwise
      waiting: false,
      selecciono: false, // true si selecciono la celda de origen
      sugerencia: "", // String para el historial de sugerencias
      adyacentesSugerencia: 0 //Cantidad de adyacentes que toma a partir de una sugerencia
    };
    this.handleClick = this.handleClick.bind(this);
    this.handlePengineCreate = this.handlePengineCreate.bind(this);
    this.pengine = new PengineClient(this.handlePengineCreate);
  }

  handlePengineCreate() {
    const queryS = 'init(Grid)';
    this.pengine.query(queryS, (success, response) => {
      if (success) {
        this.setState({
          grid: response['Grid']
        });
        
      } 
    });
  }
  
  handleClick(color) {
    // No action on click if game is complete or we are waiting.
    if (this.state.waiting) {
     // alert("Se está evaluando una jugada. Por favor, espere para continuar con el juego...");
      return;
    } else if(this.state.complete) {
     // alert("El juego ya está completo.");
      return;
    }
    // Build Prolog query to apply the color flick.
    // The query will be like:
    // flick([[g,g,b,g,v,y,p,v,b,p,v,p,v,r],
    //        [r,r,p,p,g,v,v,r,r,b,g,v,p,r],
    //        [b,v,g,y,b,g,r,g,p,g,p,r,y,y],
    //        [r,p,y,y,y,p,y,g,r,g,y,v,y,p],
    //        [y,p,y,v,y,g,g,v,r,b,v,y,r,g],
    //        [r,b,v,g,b,r,y,p,b,p,y,r,y,y],
    //        [p,g,v,y,y,r,b,r,v,r,v,y,p,y],
    //        [b,y,v,g,r,v,r,g,b,y,b,y,p,g],
    //        [r,b,b,v,g,v,p,y,r,v,r,y,p,g],
    //        [v,b,g,v,v,r,g,y,b,b,b,b,r,y],
    //        [v,v,b,r,p,b,g,g,p,p,b,y,v,p],
    //        [r,p,g,y,v,y,r,b,v,r,b,y,r,v],
    //        [r,b,b,v,p,y,p,r,b,g,p,y,b,r],
    //        [v,g,p,b,v,v,g,g,g,b,v,g,g,g]],r, Grid)
    const gridS = JSON.stringify(this.state.grid).replaceAll('"', "");
    const queryS = "flick(" + gridS + "," + color + ", Grid, ("+this.state.origenColor+","+this.state.origenFila+","+this.state.origenColumna+"))"; 
    this.setState({
      waiting: true
    });
    this.pengine.query(queryS, (success, response) => {
      if (success) {
        this.setState({
          grid: response['Grid'],
          origenColor: color,
          turns: this.state.turns + 1,
          jugadas: this.state.jugadas + "" + color,
          waiting: false
        });

        // Calcula la cantidad de adyacentes que hay después de pintar
        const gridNueva = JSON.stringify(this.state.grid).replaceAll('"', "");
        const queryAdyacentes = "cantidadAdyacentes(" + gridNueva + "," +"("+this.state.origenColor+","+this.state.origenFila+","+this.state.origenColumna+"), N)"; 
        this.setState({
          waiting: true
        });
        this.pengine.query(queryAdyacentes, (success, response) => {
          if (success) {
            this.setState({
              adyacentes: response['N'],
              waiting: false
            });
          }  
        });

        // Comprueba si ganó el juego
        const queryGano = "gano("+gridNueva+")"; 
        this.setState({
          waiting: true
        });
        this.pengine.query(queryGano, (success, response) => {
          if (success) {
            this.setState({
              complete: true,
              waiting: false
            });
            //alert("Ganaste el juego!");
          } else{
            this.setState({
              waiting: false
            });
          }  
        });

      } else {
        // Prolog query will fail when the clicked color coincides with that in the top left cell.
        this.setState({
          waiting: false
        });
      }
    });
  }

  clickSugerenciaEnProfundidad(numero){
    
    const gridS = JSON.stringify(this.state.grid).replaceAll('"', "");
    console.log(numero);
    
    console.log(gridS);

    const filacons = this.state.origenFila;
    const columnacons = this.state.origenColumna;
    //sugerirNVeces(M, (C,I,J), N, Ln, NAdy):- LA MATRIZ DEBERIA LLAMARLA COMO UNA LISTA DE LISTAS
    const querySugerir = "sugerirNVeces("+ gridS + ",("+ this.state.origenColor + "," +filacons+ "," +columnacons+"),"+numero+", Ln, NAdy)";
    //console.log(querySugerir); esta bien
    console.log(this.state.jugadas);

    this.pengine.query(querySugerir, (success, response) => {
      if(success){
        this.setState({
          sugerencia: JSON.stringify(response['Ln']).replaceAll(',',"").replaceAll('[',"").replaceAll(']',"").replaceAll('"',""),
          adyacentesSugerencia: response ['NAdy']
        })
      }
    })

  }

  clickSugerenciaExponencial(numero){
    
    const gridS = JSON.stringify(this.state.grid).replaceAll('"', "");
    console.log(numero);
    
    console.log(gridS);

    const filacons = this.state.origenFila;
    const columnacons = this.state.origenColumna;
    //buscarSecuencia(+M, +(C,I,J), +N, -L, -NAdy) LA MATRIZ DEBERIA LLAMARLA COMO UNA LISTA DE LISTAS
    const querySugerir = "buscarSecuencia("+ gridS + ",("+ this.state.origenColor + "," +filacons+ "," +columnacons+"),"+numero+", Ln, NAdy)";
    //console.log(querySugerir); esta bien
    console.log(this.state.jugadas);

    this.pengine.query(querySugerir, (success, response) => {
      if(success){
        this.setState({
          sugerencia: JSON.stringify(response['Ln']).replaceAll(',',"").replaceAll('[',"").replaceAll(']',"").replaceAll('"',""),
          adyacentesSugerencia: response ['NAdy']
        })
      }
    })

  }

  clickOrigen(c, i, j){
    if(this.state.selecciono === false) {
      this.setState({ 
        // En prolog la primera fila o primer columna no empieza de 0, si no de 1
        origenFila: i+1,
        origenColumna: j+1, 
        origenColor: c,
        selecciono: true
      });

      // Comprueba, por excepción, si en la grilla inicial según la celda origen que se seleccionó hay adyacentes o está "ganada" (todos las celdas son iguales) 

      // Calcula la cantidad de adyacentes que hay después de pintar
      const gridNueva = JSON.stringify(this.state.grid).replaceAll('"', "");
      console.log(gridNueva);
      const queryAdyacentes = "cantidadAdyacentes(" + gridNueva + "," +"("+c+","+ (i+1) +","+ (j+1) +"), N)"; 
      console.log(queryAdyacentes);
      this.setState({
        waiting: true
      });
      this.pengine.query(queryAdyacentes, (success, response) => {
        if (success) {
          this.setState({
            adyacentes: response['N'],
            waiting: false
          });
        }  
      });

      // Comprueba si ganó el juego
      const queryGano = "gano("+gridNueva+")"; 
      this.setState({
        waiting: true
      });
      this.pengine.query(queryGano, (success, response) => {
        if (success) {
          this.setState({
            complete: true,
            waiting: false
          });
          //alert("Ganaste el juego!");
        } else{
          this.setState({
            waiting: false
          });
        }  
      });

    }
  }

  render() {
    if (this.state.grid === null) {
      return null;
    }

    if(this.state.selecciono === true) {
      if(this.state.complete === false){ // Jugando
        return (
          <div className="game">
            <div className="leftPanel">
              <div className="buttonsPanel">
                {colors.map(color =>
                  <button
                    className="colorBtn"
                    style={{ backgroundColor: colorToCss(color) ,
                      borderRadius: '5px'
                    }}
                    onClick={() => this.handleClick(color)}
                    key={color}
                  />)}
              </div>
              <div><br></br><center><button className='boton'
                style={{ backgroundColor: '#4CAF50',
                  fontSize: '16px',
                  color: 'White',
                  borderColor: 'Black',
                  borderRadius: '5px',
                  textAlign: 'center',
                  textDecoration: 'none',
                  padding: '15px 32px'
                }}
                onClick={() => window.location.reload()}>Reiniciar juego</button></center></div>
              <div className="turnsPanel">
              <div className="adyacentesLab">Jugadas</div>
                <div className="turnsNum">{this.state.turns}</div>
                <div className="adyacentesLab">Adyacentes actuales</div>
                <div className="turnsNum">{this.state.adyacentes}</div>
                <div className='helpPanel'>
                <div className="ayuda">
                  <center>
                  <input id='cajaNum' type="number" min={1} style={{
                  borderColor: 'Black',
                  borderRadius: '5px',
                  textAlign: 'center'
                  }}></input>
                  </center>
                </div>
                <div>
                <center>
                  
                  <button className='boton' id='Ayuda' style={{ backgroundColor: '#FFFF99' ,
                      borderRadius: '5px',
                      textAlign: 'center',
                    }}onClick={() =>  this.clickSugerenciaExponencial(document.getElementById('cajaNum').value)}>Sugerencia General</button> 
                 </center>
                </div>
                
                <div>
                <center>
                <button className='boton' id='profundidad'style={{ backgroundColor: '#FFFF99' ,
                      borderRadius: '5px',
                      textAlign: 'center',
                    }}onClick={() =>  this.clickSugerenciaEnProfundidad(document.getElementById('cajaNum').value)}>Sugerencia Profundidad</button>
                </center>
                </div>
                <br></br>
                <div id='cantAdySug'>Adyacentes por sugerencia</div>
                <center>
                <div className="turnsNum">{this.state.adyacentesSugerencia}</div>
                </center>

                <div class="historialHelp"></div>
                <div class = "caja">
                  {this.state.sugerencia.split('').map((colorSugerencia) => 
                  <div 
                    style={{ backgroundColor: colorToCss(colorSugerencia),
                    width: '7px',
                    border: '1px solid #606060',
                    borderRadius: '5px',
                    padding: '10px',
                    margin: '13px'}} />
                  )}
                </div>

                
                </div>
                <div className="adyacentesLab"><br></br>Historial de jugadas</div>
                <div class="caja">
                  {this.state.jugadas.split('').map((colorHistorial) => 
                  <div 
                    style={{ backgroundColor: colorToCss(colorHistorial),
                    width: '7px',
                    border: '1px solid #606060',
                    borderRadius: '5px',
                    padding: '10px',
                    margin: '13px'}} />
                  )}
                </div>
                  
            </div>
            
            </div>
            <Board grid={this.state.grid} origenFila={this.state.origenFila} origenColumna={this.state.origenColumna} onClick={(c,i,j) => this.clickOrigen(c, i,j)}/>
            
          </div>
           
        );
      } else {
        return ( // Si el juego está completo, renderiza un cartel de "juego completo"
          <div className="game">
            <div className="leftPanel">
              <div className="buttonsPanel">
                {colors.map(color =>
                  <button
                    className="colorBtn"
                    style={{ backgroundColor: colorToCss(color) ,
                      borderRadius: '5px'
                    }}
                    onClick={() => this.handleClick(color)}
                    key={color}
                  />)}
              </div>
              <div><br></br><center><button className='boton'
                style={{ backgroundColor: '#4CAF50',
                  fontSize: '16px',
                  color: 'White',
                  borderColor: 'Black',
                  borderRadius: '5px',
                  textAlign: 'center',
                  textDecoration: 'none',
                  padding: '15px 32px'
              }}
                onClick={() => window.location.reload()}>Reiniciar juego</button></center></div>

              <div className="turnsPanel">
              <div className="adyacentesLab" style={{color: 'green'}}>Juego completado!</div>
                <div className="adyacentesLab"><br></br>Jugadas</div>
                <div className="turnsNum">{this.state.turns}</div>
                <div className="adyacentesLab">Adyacentes actuales</div>
                <div className="turnsNum">{this.state.adyacentes}</div>
                <div className="adyacentesLab"><br></br>Historial de jugadas</div>
                <div class="caja">
                  {this.state.jugadas.split('').map((colorHistorial) => 
                  <div 
                    style={{ backgroundColor: colorToCss(colorHistorial),
                    width: '7px',
                    border: '1px solid #606060',
                    borderRadius: '5px',
                    padding: '10px',
                    margin: '13px'}} />
                  )}
                </div>
                  
            </div>
            </div>
            <Board grid={this.state.grid} origenFila={this.state.origenFila} origenColumna={this.state.origenColumna} onClick={(c, i,j) => this.clickOrigen(c, i,j)}/>
            
          </div>
          
        );
      }
    } else{ // Inicio de juego
      
      setTimeout(() => { 
        if(this.state.selecciono === false) {
          this.setState({
            selecciono: true,
            origenColor: JSON.stringify(this.state.grid).charAt(3),
          });
        }
      }, 10000);


      return(
      <div className="game">
         <center>Para comenzar, selecciona la celda origen
          <Board grid={this.state.grid} origenFila={1} origenColumna={1} onClick={(c, i,j) => this.clickOrigen(c, i,j)}/>
          <i>Por defecto, la celda origen es la superior izquierda. (Si en 10 segundos no se selecciona otra celda, esta será la celda origen)</i>
          </center>
        </div>
      );
    }
  }
  // OBS: En el board se pasa la celda origen para visualizar en el tablero diferente a dicha celda.
}


export default Game;