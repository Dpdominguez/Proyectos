// Agent judge in project ESEI_SAGA.mas2j
//Roi Pérez López, Martín Puga Egea
/* ----- Initial beliefs and rules ------ */


jugadasRestantes(100).

jugadasPlayer(player1,0).
jugadasPlayer(player2,0).

turnoActual(player1).

turnoActivado(0).

fueraTablero(0).

fueraTurno(player1,0).
fueraTurno(player2,0).

jugadorDescalificado(player1,0).
jugadorDescalificado(player2,0).

level(1).
puntos(player1,1,0).
puntos(player1,2,0).
puntos(player2,1,0).
puntos(player2,2,0).
cambios(0).
puntosFin(1,200).
puntosFin(2,200).
findejuego(0).

//Comprobacion completa de las condiciones de un movimiento correcto: Seleccion, movimiento y color
movimientoValido(pos(X,Y),Dir):- tablero(celda(X,Y,_),ficha(COrigen,_)) & validacion(X,Y,Dir,COrigen).
validacion(X,Y,"up",COrigen) :- tablero(celda(X,Y-1,_),ficha(CDestino,_)) & not mismoColor(COrigen,CDestino).
validacion(X,Y,"down",COrigen) :- tablero(celda(X,Y+1,_),ficha(CDestino,_)) & not mismoColor(COrigen,CDestino).
validacion(X,Y,"left",COrigen) :- tablero(celda(X-1,Y,_),ficha(CDestino,_)) & not mismoColor(COrigen,CDestino).
validacion(X,Y,"right",COrigen) :- tablero(celda(X+1,Y,_),ficha(CDestino,_)) & not mismoColor(COrigen,CDestino).
mismoColor(COrigen,CDestino) :- COrigen=CDestino.

//Comprobacion de Movimiento
direccionCorrecta(pos(X,Y),Dir):- tablero(celda(X,Y,_),_) & movimiento(X,Y,Dir).
movimiento(X,Y,"up") :- tablero(celda(X,Y-1,_),_).
movimiento(X,Y,"down") :- tablero(celda(X,Y+1,_),_).
movimiento(X,Y,"left") :- tablero(celda(X-1,Y,_),_).
movimiento(X,Y,"right") :- tablero(celda(X+1,Y,_),_).

//Comprobacion de Color
colorFichasDistintos(pos(X,Y),Dir):- tablero(celda(X,Y,_),ficha(COrigen,_)) & validacion(X,Y,Dir,COrigen).

//Parte de la generacion aleatoria del tipo de ficha
codFicha(Rand,Ficha):-
	(Rand == 0 & Ficha = ip) | (Rand == 1 & Ficha = in) | (Rand == 2 & Ficha = ct) | (Rand == 3 & Ficha = gs)
	| (Rand == 4 & Ficha = co).

//Duenho de la jugada
plNumb(A,PlNumb):-
	(A == player1 & PlNumb = 1) | (A == player2 & PlNumb = 2).


//Calculo de coordenadas para un movimiento
nextMove(P1,P2,NX,NY,Dir):-
	(
	(Dir == "up" & NX = P1 & NY= (P2 - 1)) |
	(Dir == "down" & NX = P1 & NY = (P2 + 1)) |
	(Dir == "right" & NX = (P1 + 1) & NY = P2) |
	(Dir == "left" & NX = (P1 - 1) & NY = P2)
	).

	
//Comprobacipon agrupaciones	
compruebaT(X,Y):- plotioTAR(X,Y)|plotioTAB(X,Y)|plotioTD(X,Y)|plotioTI(X,Y).

plotioTAR(X,Y):- tablero(celda(X,Y,_),ficha(ColorO,_))& 
							tablero(celda(X-1,Y,_),ficha(ColorO,_))& 
							tablero(celda(X+1,Y,_),ficha(ColorO,_))&
							tablero(celda(X,Y-1,_),ficha(ColorO,_))& 
							tablero(celda(X,Y-2,_),ficha(ColorO,_)).

plotioTAB(X,Y):- tablero(celda(X,Y,_),ficha(ColorO,_))& 
							tablero(celda(X-1,Y,_),ficha(ColorO,_))& 
							tablero(celda(X+1,Y,_),ficha(ColorO,_))&
							tablero(celda(X,Y+1,_),ficha(ColorO,_))&
							tablero(celda(X,Y+2,_),ficha(ColorO,_)).

plotioTD(X,Y):- tablero(celda(X,Y,_),ficha(ColorO,_))& 
							 tablero(celda(X,Y-1,_),ficha(ColorO,_))& 
							 tablero(celda(X,Y+1,_),ficha(ColorO,_))&
							 tablero(celda(X+1,Y,_),ficha(ColorO,_))& 
							 tablero(celda(X+2,Y,_),ficha(ColorO,_)).

plotioTI(X,Y):- tablero(celda(X,Y,_),ficha(ColorO,_))& 
							 tablero(celda(X,Y-1,_),ficha(ColorO,_))& 
							 tablero(celda(X,Y+1,_),ficha(ColorO,_))&
							 tablero(celda(X-1,Y,_),ficha(ColorO,_))&
							 tablero(celda(X-2,Y,_),ficha(ColorO,_)).
						
//comprobacion de cinco en linea
comprueba5Linea(X,Y):- plotio5H(X,Y)| plotio5V(X,Y).	
						
						
plotio5H(X,Y):- tablero(celda(X,Y,_),ficha(ColorO,_))& 
							tablero(celda(X,Y-1,_),ficha(ColorO,_))& 
							tablero(celda(X,Y-2,_),ficha(ColorO,_))&
							tablero(celda(X+1,Y,_),ficha(ColorO,_))& 
							tablero(celda(X+2,Y,_),ficha(ColorO,_)).
							
plotio5V(X,Y):- tablero(celda(X,Y,_),ficha(ColorO,_))& 
							 tablero(celda(X,Y-1,_),ficha(ColorO,_))& 
							 tablero(celda(X,Y-2,_),ficha(ColorO,_))&
							 tablero(celda(X,Y+1,_),ficha(ColorO,_))& 
							 tablero(celda(X,Y+2,_),ficha(ColorO,_)).

							 
//comprobacion de cuatro en cubo							 
compruebaCubo(X,Y):- plotioCuboABI(X,Y)|plotioCuboABD(X,Y)|plotioCuboARI(X,Y)|plotioCuboARD(X,Y).


plotioCuboABI(X,Y):- tablero(celda(X,Y,_),ficha(ColorO,_))&
						tablero(celda(X-1,Y,_),ficha(ColorO,_))&
						tablero(celda(X-1,Y+1,_),ficha(ColorO,_))&
						tablero(celda(X,Y+1,_),ficha(ColorO,_)).

plotioCuboABD(X,Y):- tablero(celda(X,Y,_),ficha(ColorO,_))&
						tablero(celda(X+1,Y,_),ficha(ColorO,_))&
						tablero(celda(X+1,Y+1,_),ficha(ColorO,_))&
						tablero(celda(X,Y+1,_),ficha(ColorO,_)).

plotioCuboARI(X,Y):- tablero(celda(X,Y,_),ficha(ColorO,_))&
						tablero(celda(X-1,Y,_),ficha(ColorO,_))&
						tablero(celda(X-1,Y-1,_),ficha(ColorO,_))&
						tablero(celda(X,Y-1,_),ficha(ColorO,_)).

plotioCuboARD(X,Y):- tablero(celda(X,Y,_),ficha(ColorO,_))&
						tablero(celda(X+1,Y,_),ficha(ColorO,_))&
						tablero(celda(X+1,Y-1,_),ficha(ColorO,_))&
						tablero(celda(X,Y-1,_),ficha(ColorO,_)).



//comprobacion de cuatro en linea						
comprueba4Linea(X,Y,Dir):- plotio4I(X,Y)| plotio4D(X,Y)|plotio4AR(X,Y)| plotio4AB(X,Y).
							

plotio4I(X,Y):- tablero(celda(X,Y,_),ficha(ColorO,_))&
									tablero(celda(X-1,Y,_),ficha(ColorO,_))&
									tablero(celda(X-2,Y,_),ficha(ColorO,_))&
									tablero(celda(X+1,Y,_),ficha(ColorO,_)).
		
plotio4D(X,Y):- tablero(celda(X,Y,_),ficha(ColorO,_))&
									tablero(celda(X-1,Y,_),ficha(ColorO,_))&
									tablero(celda(X+1,Y,_),ficha(ColorO,_))&
									tablero(celda(X+2,Y,_),ficha(ColorO,_)).
							
					
plotio4AR(X,Y):- tablero(celda(X,Y,_),ficha(ColorO,_))&
									tablero(celda(X,Y-1,_),ficha(ColorO,_))&
									tablero(celda(X,Y-2,_),ficha(ColorO,_))&
									tablero(celda(X,Y+1,_),ficha(ColorO,_)).									
									
plotio4AB(X,Y):- tablero(celda(X,Y,_),ficha(ColorO,_))&
									tablero(celda(X,Y-1,_),ficha(ColorO,_))&
									tablero(celda(X,Y+1,_),ficha(ColorO,_))&
									tablero(celda(X,Y+2,_),ficha(ColorO,_)).
							

									
//comprobacion de tres en linea
compruebaTres(X,Y):- plotioUD(X,Y)|plotioLR(X,Y)|plotioL(X,Y)|plotioR(X,Y)|plotioU(X,Y)|plotioD(X,Y).
						
plotioUD (X,Y):- tablero(celda(X,Y,_),ficha(ColorO,_))&
							tablero(celda(X,Y-1,_),ficha(ColorO,_))&
							tablero(celda(X,Y+1,_),ficha(ColorO,_)). 	
	
plotioLR (X,Y):- tablero(celda(X,Y,_),ficha(ColorO,_))&
							tablero(celda(X-1,Y,_),ficha(ColorO,_))&
							tablero(celda(X+1,Y,_),ficha(ColorO,_)).
							
plotioL (X,Y):- tablero(celda(X,Y,_),ficha(ColorO,_))&
							tablero(celda(X-1,Y,_),ficha(ColorO,_))&
							tablero(celda(X-2,Y,_),ficha(ColorO,_)).
							
plotioR (X,Y):- tablero(celda(X,Y,_),ficha(ColorO,_))&
							tablero(celda(X+1,Y,_),ficha(ColorO,_))&
							tablero(celda(X+2,Y,_),ficha(ColorO,_)).
							
plotioU (X,Y):- tablero(celda(X,Y,_),ficha(ColorO,_))&
							tablero(celda(X,Y-1,_),ficha(ColorO,_))&
							tablero(celda(X,Y-2,_),ficha(ColorO,_)).
							
plotioD (X,Y):- tablero(celda(X,Y,_),ficha(ColorO,_))&
							tablero(celda(X,Y+1,_),ficha(ColorO,_))&
							tablero(celda(X,Y+2,_),ficha(ColorO,_)).
	
	
vacioFila:- tablero(celda(X,0,_),vacio).		
vacioDebajo(X,Y):- tablero(celda(X,Y,_),ficha(_,_)) &tablero(celda(X,Y+1,_),vacio).	
vacioIzquierda(X,Y):- tablero(celda(X,Y,_),ficha(_,_)) & tablero(celda(X-1,Y,_),vacio).

ganador(1,none,0).
ganador(2,none,0).

/* ----- Initial goals ----- */

!startGame.


/* ----- Plans ----- */



//final del juego
+!comienzoTurno:findejuego(N)& N=1<- !declararGanadorPuntos;
!declararGanadorPartida.


+!declararGanadorPuntos:puntos(player1,L,PuntosUno) & puntos(player2,L,PuntosDos)& (PuntosUno > PuntosDos)<-
.print("El nivel ", L," ha acabado, gana: player1 con ",PuntosUno," puntos ");
-ganador(L,none,0);
+ganador(L,player1,PuntosUno)
.

+!declararGanadorPuntos:level(L) &puntos(player1,L,PuntosUno) & puntos(player2,L,PuntosDos)& (PuntosUno < PuntosDos)<-
.print("El nivel ", L," ha acabado, gana: player2 con ",PuntosDos," puntos ");
-ganador(L,none,0);
+ganador(L,player2,PuntosDos)
.

+!declararGanadorPuntos:level(L) &puntos(player1,L,PuntosUno) & puntos(player2,L,PuntosDos)& (PuntosUno == PuntosDos)<-
.print("El nivel ", L," ha acabado, ha sido empate con ",PuntosUno," puntos ");
-ganador(L,none,0);
+ganador(L,empate,PuntosUno)
.

+!declararGanadorPartida:ganador(1,Ganador,Puntos) & ganador(2,Ganador2,Puntos2) & (Ganador==Ganador2)<-
	.print("El ganador es ", Ganador," con ",Puntos, " puntos en el nivel 1 y ",Puntos2," en el nivel 2").
	
+!declararGanadorPartida:ganador(1,Ganador,Puntos) & ganador(2,Ganador2,Puntos2) & not(Ganador==Ganador2)<-
	.print("Ha sido empate, cada jugador gan� un nivel, el nivel 1 ", Ganador," con ",Puntos," puntos y el nivel 2 ",Ganador2," con ", Puntos2).
+!declararGanadorPartida.	




/* COMIENZO INTOCABLE */

//Comienzo del turno de un jugador.
+!comienzoTurno : jugadorDescalificado(player1,1) & jugadorDescalificado(player2,1) <-
			.print("FIN DE LA PARTIDA: Ambos jugadores han sido descalificados. TRAMPOSOS!!!").

+!comienzoTurno : turnoActual(P) & jugadasRestantes(N) & N>0 & jugadasPlayer(P,J) & J<50 <-
	.print("Turno de: ",P,"!");
	-+turnoActivado(1);
	.print(P,", puedes mover");
	.send(P,tell,puedesMover);
	.send(P,untell,puedesMover).

+!comienzoTurno : jugadasRestantes(N) & N=0 <- .print("FIN DE LA PARTIDA: Se ha realizado el numero maximo de jugadas").


+!comienzoTurno : turnoActual(P) & jugadasPlayer(P,J) & J>=50 <- .print("FIN DE LA PARTIDA: ",P," ha realizado el maximo de jugadas por jugador (50)").


+!comienzoTurno <- .print("DEBUG: Error en +!comienzoTurno"). //Salvaguarda

+moverDesdeEnDireccion(pos(X,Y),Dir)[source(P)] : 
	turnoActual(P) & movimientoValido(pos(X,Y),Dir) & turnoActivado(1) <-
			-+turnoActivado(0);
			-moverDesdeEnDireccion(pos(X,Y),Dir)[source(P)];
			.print("Jugada valida!")
			.send(P,tell,valido);
			.send(P,untell,valido);
			+intercambiarFichas(X,Y,Dir,P);
			-intercambiarFichas(X,Y,Dir,P);
			-+turnoTerminado(P);
			!comienzoTurno.

//Movimiento Incorrecto
+moverDesdeEnDireccion(pos(X,Y),Dir)[source(P)] : 
	turnoActual(P) & not movimientoValido(pos(X,Y),Dir) & turnoActivado(1)<-
			-moverDesdeEnDireccion(pos(X,Y),Dir)[source(P)];
			-+turnoActivado(0);
			+movimientoInvalido(pos(X,Y),Dir,P).

//Movimiento realizado por un jugador que tiene el turno pero el juez aun no le ha ordenado mover
+moverDesdeEnDireccion(pos(X,Y),Dir)[source(P)] : 
	turnoActual(P) & turnoActivado(0) <-
			-moverDesdeEnDireccion(pos(X,Y),Dir)[source(P)];
			.print("Agente ",P,", espera mi orden para realizar el siguiente movimiento. No intentes mover mas de una vez.").


//Movimiento realizado por un jugador fuera de su turno
+moverDesdeEnDireccion(pos(X,Y),Dir)[source(P)] : 
	not turnoActual(P) & fueraTurno(P,N) <-
		-moverDesdeEnDireccion(pos(X,Y),Dir)[source(P)];
		.print(P," Has intentado realizar un movimiento fuera de tu turno. ", N+1," aviso");
		.send(P,tell,invalido(fueraTurno,N+1));
		.send(P,untell,invalido(fueraTurno,N+1));
		-fueraTurno(P,N);
		+fueraTurno(P,N+1).

//Descalificacion de un jugador
+fueraTurno(P,N) : N>3 <-
		-jugadorDescalificado(P,0);
		+jugadorDescalificado(P,1);
		.print("AVISO: ",P," ha sido descalificado por tramposo!!!").



//Deteccion de un agente externo a la partida (distinto a player1 y player 2) que esta intentando jugar.
// Esta regla la podeis adecuar a vuestras necesidades

+moverDesdeEnDireccion(pos(X,Y),Dir)[source(P)] : 
	not turnoActual(P) & not fueraTurno(P,N) <- // --- TODO ---
		-moverDesdeEnDireccion(pos(X,Y),Dir)[source(P)];
		.print("El agente ",P," externo a la partida está intentando jugar.").

// Esta regla la podeis adecuar a vuestras necesidades
+moverDesdeEnDireccion(pos(X,Y),Dir)[source(P)] <- 
	-moverDesdeEnDireccion(pos(X,Y),Dir)[source(P)];
	.print("DEBUG: Error en +moverDesdeEnDireccion. Source", P). //Salvaguarda

//Comprobacion de la falta cometida por mover una ficha hacia una posicion fuera del tablero, intentar mover una ficha de una posicion inexistente, o realizar un tipo de movimiento desconocido
+movimientoInvalido(pos(X,Y),Dir,P):
	fueraTablero(V) & not direccionCorrecta(pos(X,Y),Dir)  <-
		-movimientoInvalido(pos(X,Y),Dir,P);
		.print("Movimiento Invalido. Has intentado mover una ficha fuera del tablero");
		.send(P,tell,invalido(fueraTablero,V+1));
		.send(P,untell,invalido(fueraTablero,V+1));
		-+turnoActivado(1);
		-+fueraTablero(V+1).

//Comprobacion de la falta cometida por intercambiar dos fichas del mismo color
+movimientoInvalido(pos(X,Y),Dir,P) : 
	not colorFichasDistintos(pos(X,Y),Dir) <-
		-movimientoInvalido(pos(X,Y),Dir,P);
		.print("Movimiento Invalido. Has intentado  intercambiar dos fichas del mismo color");
		-+turnoActivado(1);
		.print("Intentalo de nuevo!");
		.send(P,tell,tryAgain);
		.send(P,untell,tryAgain).

// Esta regla la podeis adecuar a vuestras necesidades
+movimientoInvalido(pos(X,Y),Dir,P) <- 
	.print("DEBUG: Error en +movimientoInvalido").


//Recepcion del aviso de que un jugador pasa turno por haber realizado un movimiento fuera del tablero mas de 3 veces
+pasoTurno[source(P)] : turnoActual(P) <-
		-+fueraTablero(0);
		.print(P," ha pasado turno");
		+cambioTurno(P);
		!comienzoTurno.
			
			

/* FIN INTOCABLE */





+!startGame <- .print("Tablero de juego generado!");
				+mostrarTablero(player1);
				-mostrarTablero(player1);
				+mostrarTablero(player2);
				-mostrarTablero(player2);
				?level(Level);
				?size(Tam);
				.send(player1,tell,level(Level));
				.send(player2,tell,level(Level));
				.send(player1,tell,size(Tam));
				.send(player2,tell,size(Tam));
				.print("EMPIEZA EL JUEGO!")
				!comienzoTurno.


 //Comunicacion del tablero al jugador indicado.
+mostrarTablero(P) : size(N) <- .findall(tablero(X,Y),tablero(X,Y),Lista);
		for ( .member(Estructure,Lista) ) {
			.send(P,tell,Estructure);
		 };
		 .send(P,tell,size(N)).




//Cambio de turno de un jugador a otro
+cambioTurno(P) : jugadasRestantes(N) & jugadasPlayer(P,J)<-
				-cambioTurno(P);
				+cambioTurno(P,N,J).


+cambioTurno(P,N,J) : P = player1 | jugadorDescalificado(player1,1) <-
					-cambioTurno(P,N,J);
					-+turnoActual(player2);
					-+jugadasRestantes(N-1);
					-jugadasPlayer(player1,J);
					+jugadasPlayer(player1,J+1);
					.print("[ Jugadas restantes: ", N-1," || Jugadas completadas ",P,": ", J+1," ]").
					


+cambioTurno(P,N,J) : P = player2 | jugadorDescalificado(player2,1)<-
					-cambioTurno(P,N,J);
					-+turnoActual(player1);
					-+jugadasRestantes(N-1);
					-jugadasPlayer(player2,J);
					+jugadasPlayer(player2,J+1);
					.print("[ Jugadas restantes: ", N-1," || Jugadas completadas ",P,": ", J+1," ]").


//Cambio de turno cuando hay un jugador descalificado
+cambioTurnoMismoJugador(P):jugadasRestantes(N) & jugadasPlayer(P,J)<-
				-cambioTurnoMismoJugador(P);
				+cambioTurnoMismoJugador(P,N,J).
+cambioTurnoMismoJugador(P,N,J) <-
					-cambioTurnoMismoJugador(P,N,J);
					-+jugadasRestantes(N-1);
					-jugadasPlayer(P,J);
					+jugadasPlayer(P,J+1);
					.print("[ Jugadas restantes: ", N-1," || Jugadas completadas ",P,": ", J+1," ]").




+turnoTerminado(P):level(L)& L=1 &((puntosFin(L,PuntosFin)& puntos(P,L,Puntos)& Puntos>=PuntosFin)| (jugadasRestantes(N) & N=1) |  (jugadasPlayer(P,J) & J>49))<-
					!declararGanadorPuntos;
					.send(player1,untell,level(1));
					.send(player2,untell,level(1));
					.send(player1,tell,level(2));
					.send(player2,tell,level(2));
					.wait(1500);
					-+level(2);
					-+turnoActual(player1);
					-+jugadasRestantes(100);
					-jugadasPlayer(player1,_);
					+jugadasPlayer(player1,0);
					-jugadasPlayer(player2,_);
					+jugadasPlayer(player2,0);
					.abolish(tablero(X,Y));
					.wait(250);
					nivel(2);
					.wait(250);
					!actuTableroPlayers.



+turnoTerminado(P):level(L)& L=2 &((puntosFin(L,PuntosFin)& puntos(P,L,Puntos)& Puntos>=PuntosFin)| (jugadasRestantes(Jug) & Jug=1 ) | (jugadasPlayer(P,J) & J>49))<-
!declararGanadorPuntos;
-findejuego(0);
+findejuego(1).

+turnoTerminado(P): jugadorDescalificado(J,B) & B=1 <- +cambioTurnoMismoJugador(P).

+turnoTerminado(P) <- +cambioTurno(P);.

+intercambiarFichas(X,Y,Dir,P) : nextMove(X,Y,NX,NY,Dir) & plNumb(P,PlNumb) <-

								-tablero(celda(X,Y,_),X1);
								-tablero(celda(NX,NY,_),X2);
								+tablero(celda(NX,NY,0),X1);
								+tablero(celda(X,Y,0),X2);
								-+ultimoMov(X,Y,Dir,P);
								intercambiarFichas(X,Y,NX,NY);
								.print("Se han intercambiado las fichas entre las posiciones (",X,",",Y,") y (",NX,",",NY,")");
								!compruebaAgru(X,Y);
								!compruebaAgru(NX,NY);
								!caida;
								!revisarPatrones;
								!actuTableroPlayers.
								

								
								
+!actuTableroPlayers<-
	.send(player1,tell,borraTablero);
	.send(player2,tell,borraTablero);
	.findall(tablero(X,Y),tablero(X,Y),Resul);
	for(.member(Celda,Resul)){
		.send(player1,tell,Celda);
		.send(player2,tell,Celda);
	}.
+!actuTableroPlayers.

+!compruebaAgru(X,Y):plotioTAR(X,Y)<-?tablero(celda(X,Y,_),ficha(Color,_));
!explotaFicha(X,Y);
!explotaFicha(X-1,Y);
!explotaFicha(X+1,Y);
!explotaFicha(X,Y-1);
!explotaFicha(X,Y-2);
!fichaSpecial(X,Y,co,Color)
.


+!compruebaAgru(X,Y):plotioTAB(X,Y)<-?tablero(celda(X,Y,_),ficha(Color,_));
!explotaFicha(X,Y);
!explotaFicha(X-1,Y);
!explotaFicha(X+1,Y);
!explotaFicha(X,Y+1);
!explotaFicha(X,Y+2);
!fichaSpecial(X,Y,co,Color)
.
+!compruebaAgru(X,Y):plotioTD(X,Y)<-?tablero(celda(X,Y,_),ficha(Color,_));
!explotaFicha(X,Y);
!explotaFicha(X+1,Y);
!explotaFicha(X+2,Y);
!explotaFicha(X,Y-1);
!explotaFicha(X,Y+1);
!fichaSpecial(X,Y,co,Color)
.
+!compruebaAgru(X,Y):plotioTI(X,Y)<-?tablero(celda(X,Y,_),ficha(Color,_));
!explotaFicha(X,Y);
!explotaFicha(X-1,Y);
!explotaFicha(X-2,Y);
!explotaFicha(X,Y-1);
!explotaFicha(X,Y+1);
!fichaSpecial(X,Y,co,Color)
.
+!compruebaAgru(X,Y):plotio5H(X,Y)<-?tablero(celda(X,Y,_),ficha(Color,_));
!explotaFicha(X,Y);
!explotaFicha(X,Y-1);
!explotaFicha(X,Y-2);
!explotaFicha(X,Y+1);
!explotaFicha(X,Y+2);
!fichaSpecial(X,Y,ct,Color)
.
+!compruebaAgru(X,Y):plotio5V(X,Y)<-?tablero(celda(X,Y,_),ficha(Color,_));
!explotaFicha(X,Y);
!explotaFicha(X-1,Y);
!explotaFicha(X-2,Y);
!explotaFicha(X+1,Y);
!explotaFicha(X+2,Y);
!fichaSpecial(X,Y,ct,Color).

+!compruebaAgru(X,Y):plotioCuboABI(X,Y)<-?tablero(celda(X,Y,_),ficha(Color,_));
!explotaFicha(X,Y);
!explotaFicha(X-1,Y);
!explotaFicha(X-1,Y+1);
!explotaFicha(X,Y+1);
!fichaSpecial(X,Y,gs,Color).

+!compruebaAgru(X,Y):plotioCuboABD(X,Y)<-?tablero(celda(X,Y,_),ficha(Color,_));
!explotaFicha(X,Y);
!explotaFicha(X+1,Y);
!explotaFicha(X+1,Y+1);
!explotaFicha(X,Y+1);
!fichaSpecial(X,Y,gs,Color).

+!compruebaAgru(X,Y):plotioCuboARI(X,Y)<-?tablero(celda(X,Y,_),ficha(Color,_));
!explotaFicha(X,Y);
!explotaFicha(X-1,Y);
!explotaFicha(X-1,Y-1);
!explotaFicha(X,Y-1);
!fichaSpecial(X,Y,gs,Color).

+!compruebaAgru(X,Y):plotioCuboARD(X,Y)<-?tablero(celda(X,Y,_),ficha(Color,_));
!explotaFicha(X,Y);
!explotaFicha(X+1,Y);
!explotaFicha(X+1,Y-1);
!explotaFicha(X,Y-1);
!fichaSpecial(X,Y,gs,Color).

+!compruebaAgru(X,Y):plotio4I(X,Y)<-?tablero(celda(X,Y,_),ficha(Color,_));
!explotaFicha(X,Y);
!explotaFicha(X-1,Y);
!explotaFicha(X-2,Y);
!explotaFicha(X+1,Y);
!fichaSpecial(X,Y,ip,Color).

+!compruebaAgru(X,Y):plotio4D(X,Y)<-?tablero(celda(X,Y,_),ficha(Color,_));
!explotaFicha(X,Y);
!explotaFicha(X+1,Y);
!explotaFicha(X+2,Y);
!explotaFicha(X-1,Y);
!fichaSpecial(X,Y,ip,Color).

+!compruebaAgru(X,Y):plotio4AR(X,Y)<-?tablero(celda(X,Y,_),ficha(Color,_));
!explotaFicha(X,Y);
!explotaFicha(X,Y-1);
!explotaFicha(X,Y-2);
!explotaFicha(X,Y+1);
!fichaSpecial(X,Y,ip,Color).

+!compruebaAgru(X,Y):plotio4AB(X,Y)<-?tablero(celda(X,Y,_),ficha(Color,_));
!explotaFicha(X,Y);
!explotaFicha(X,Y+1);
!explotaFicha(X,Y+2);
!explotaFicha(X,Y-1);
!fichaSpecial(X,Y,ip,Color).

+!compruebaAgru(X,Y):plotioUD(X,Y)<-?tablero(celda(X,Y,_),ficha(Color,_));
!explotaFicha(X,Y);
!explotaFicha(X,Y-1);
!explotaFicha(X,Y+1).

+!compruebaAgru(X,Y):plotioLR(X,Y)<-?tablero(celda(X,Y,_),ficha(Color,_));
!explotaFicha(X,Y);
!explotaFicha(X-1,Y);
!explotaFicha(X+1,Y).

+!compruebaAgru(X,Y):plotioL(X,Y)<-?tablero(celda(X,Y,_),ficha(Color,_));
!explotaFicha(X,Y);
!explotaFicha(X-1,Y);
!explotaFicha(X-2,Y).

+!compruebaAgru(X,Y):plotioR(X,Y)<-?tablero(celda(X,Y,_),ficha(Color,_));
!explotaFicha(X,Y);
!explotaFicha(X+1,Y);
!explotaFicha(X+2,Y).

+!compruebaAgru(X,Y):plotioU(X,Y)<-?tablero(celda(X,Y,_),ficha(Color,_));
!explotaFicha(X,Y);
!explotaFicha(X,Y-1);
!explotaFicha(X,Y-2).

+!compruebaAgru(X,Y):plotioD(X,Y)<-?tablero(celda(X,Y,_),ficha(Color,_));
!explotaFicha(X,Y);
!explotaFicha(X,Y+1);
!explotaFicha(X,Y+2).

+!compruebaAgru(X,Y).


+!explotaFicha(X,Y):tablero(celda(X,Y,_),ficha(Color,Tipo))& (Tipo==in)& level(L)&ultimoMov(_,_,_,UP) <-
				delete(X,Y);
				-tablero(celda(X,Y,_),ficha(Color,Tipo));
				+tablero(celda(X,Y,0),vacio);
				-puntos(UP,L,Pun);
				+puntos(UP,L,Pun+1).


+!explotaFicha(X,Y):tablero(celda(X,Y,_),ficha(_,Tipo))& (Tipo==ip)& level(L)&ultimoMov(_,_,UDir,UP)<-
				delete(X,Y);
				-tablero(celda(X,Y,_),ficha(_,Tipo));
				+tablero(celda(X,Y,0),vacio);
				-puntos(UP,L,Pun);
				+puntos(UP,L,Pun+2);
				//toda la fila o toda la columna
				!explotaFichaip(X,Y,UDir).

+!explotaFichaip(X,Y,UDir):UDir=="up" | UDir=="down"<- //explota toda la columna 
					?size(Tam);
					for(.range(YY,0,(Tam-1))){
						!explotaFicha(X,YY);
					}.
+!explotaFichaip(X,Y,UDir):UDir=="left" | UDir=="right"<-//explota toda la fila
					?size(Tam);
					for(.range(XX,0,(Tam-1))){
						!explotaFicha(XX,Y);
					}.
					
+!explotaFichaip(X,Y,UDir).					
					
					
+!explotaFicha(X,Y):tablero(celda(X,Y,_),ficha(Color,Tipo))& (Tipo==gs)& level(L)&ultimoMov(_,_,_,UP)<-
				delete(X,Y);
				-tablero(celda(X,Y,_),ficha(Color,Tipo));
				+tablero(celda(X,Y,0),vacio);
				-puntos(UP,L,Pun);
				+puntos(UP,L,Pun+4);
				//una ficha de ese color
				!explotaFichaMismoColor(Color);
				
.

+!explotaFichaMismoColor(Color):tablero(celda(XE,YE,_),ficha(Color,_))<-
				!explotaFicha(XE,YE).

+!explotaFichaMismoColor(Color).


+!explotaFicha(X,Y):tablero(celda(X,Y,_),ficha(_,Tipo))& (Tipo==co)& level(L)&ultimoMov(_,_,_,UP)<-
				delete(X,Y);
				-tablero(celda(X,Y,_),ficha(_,Tipo));
				+tablero(celda(X,Y,0),vacio);
				-puntos(UP,L,Pun);
				+puntos(UP,L,Pun+6);	
				//cubo3*3 al rededor de X,Y
				!explotaFicha(X+1,Y);
				!explotaFicha(X-1,Y);
				!explotaFicha(X,Y+1);
				!explotaFicha(X,Y-1);
				!explotaFicha(X+1,Y-1);
				!explotaFicha(X+1,Y+1);
				!explotaFicha(X-1,Y-1);
				!explotaFicha(X-1,Y+1).

+!explotaFicha(X,Y):tablero(celda(X,Y,_),ficha(Color,Tipo))& (Tipo==ct)& level(L)&ultimoMov(_,_,_,UP)<-
				delete(X,Y);
				-tablero(celda(X,Y,_),ficha(Color,Tipo));
				+tablero(celda(X,Y,0),vacio);
				-puntos(UP,L,Pun);
				+puntos(UP,L,Pun+8);	
				//todas las fichas del color
				for(tablero(celda(NX,NY,_),ficha(Color,_))){
						!explotaFicha(NX,NY);
					}.

+!explotaFicha(X,Y).


+!fichaSpecial(X,Y,Tipo,Color):Tipo==co & level(L) &ultimoMov(_,_,_,UP)<-
-puntos(UP,L,Pun);
+puntos(UP,L,Pun+6);
-tablero(celda(X,Y,_),vacio);
+tablero(celda(X,Y,0),ficha(Color,Tipo));
put(X,Y,Color,Tipo).

+!fichaSpecial(X,Y,Tipo,Color):Tipo==gs& level(L) &ultimoMov(_,_,_,UP)<-
-puntos(UP,L,Pun);
+puntos(UP,L,Pun+4);
-tablero(celda(X,Y,_),vacio);
+tablero(celda(X,Y,0),ficha(Color,Tipo));
put(X,Y,Color,Tipo).

+!fichaSpecial(X,Y,Tipo,Color):Tipo==ip& level(L) &ultimoMov(_,_,_,UP)<-
-puntos(UP,L,Pun);
+puntos(UP,L,Pun+2);
-tablero(celda(X,Y,_),vacio);
+tablero(celda(X,Y,0),ficha(Color,Tipo));
put(X,Y,Color,Tipo).

+!fichaSpecial(X,Y,Tipo,Color):Tipo==ct& level(L) &ultimoMov(_,_,_,UP)<-
-puntos(UP,L,Pun);
+puntos(UP,L,Pun+8);
-tablero(celda(X,Y,_),vacio);
+tablero(celda(X,Y,0),ficha(Color,Tipo));
put(X,Y,Color,Tipo).



+!caida:(vacioFila | vacioDebajo(_,_) )& size(Tam)<-
		for(.range(X,0,Tam-1)){
			for(.range(Y,0,Tam-1)){
				!caete(X,Y);
			};
		};
		!rellenarFichas;
		!caida.
	
+!caida.

+!caete(X,Y): tablero(celda(X,Y,_),ficha(Color,Tipo)) & vacioDebajo(X,Y)<-

-tablero(celda(X,Y,_),ficha(Color,Tipo));
-tablero(celda(X,Y+1,_),vacio);
+tablero(celda(X,Y+1,0),ficha(Color,Tipo));
+tablero(celda(X,Y,0),vacio);
delete(X,Y);
put(X,Y+1,Color,Tipo);
-hacaido(0);
+hacaido(1);
!caete(X,Y+1).

+!caete(X,Y): tablero(celda(X,Y,_),ficha(Color,Tipo)) & not vacioDebajo(X,Y)<-
			!rueda(X,Y).
			
+!caete(X,Y).

+!rueda(X,Y):tablero(celda(X,Y,_),ficha(Color,Tipo)) &  vacioIzquierda(X,Y) & hacaido(1)<-

			-tablero(celda(X,Y,_),ficha(Color,Tipo));
			-tablero(celda(X-1,Y,_),vacio);
			+tablero(celda(X-1,Y,0),ficha(Color,Tipo));
			+tablero(celda(X,Y,0),vacio);
			delete(X,Y);
			put(X-1,Y,Color,Tipo);
			
			!rueda(X-1,Y).
			
+!rueda(X,Y)<--hacaido(1);
+hacaido(0).

+!rellenarFichas:tablero(celda(_,0,_),vacio)<-
		
				for(tablero(celda(I,0,_),vacio)){
				.random(Color,10);	
				-tablero(celda(I,0,_),vacio);
				+tablero(celda(I,0,0),ficha(math.round(5*Color),in));
				put(I,0,math.round(5*Color),in);
				};
				-cambios(0);
				+cambios(1);
				!caida.
				
+!rellenarFichas.

+!revisarPatrones:cambios(Cam) & Cam==1 & size(N)<-
	-cambios(1);
	+cambios(0);
	for ( .range(I,0,(N-1)) ) {
			for ( .range(J,0,(N-1)) ) {
				!compruebaAgru(X,Y);
				!caida;
			}
	}
	!revisarPatrones.

+!revisarPatrones.



+tablero(CeldaL,FichaL)[source(percept)]<-
	-tablero(CeldaL,FichaL)[source(percept)];
	+tablero(CeldaL,FichaL);
.


//Plan por defecto a ejecutar en caso desconocido.
+Default[source(A)]: not A=self  <- .print("El agente ",A," se comunica conmigo, pero no lo entiendo!").
