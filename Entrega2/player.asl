// Agent player1 in project practica1.mas2j



/* Initial beliefs and rules */


mejorMovPuntos(0,0,"right",0).

movRandom(0,0,"right",0).
puntos(0).


//comprobacion de si ese movimiento es valido
movimientoValido(pos(X,Y),Dir):- validoMov(pos(X,Y),Dir) & compColor(pos(X,Y),Dir) & compObs(pos(X,Y),Dir) & compVacio(pos(X,Y),Dir).

//Comprobacion completa de las condiciones de un movimiento correcto: Seleccion, movimiento y color
compColor(pos(X,Y),Dir):- tablero(celda(X,Y,_),ficha(COrigen,_)) & validacionColor(X,Y,Dir,COrigen).
validacionColor(X,Y,"left",COrigen) :- tablero(celda(X-1,Y,_),ficha(CDestino,_)) & not mismoColor(COrigen,CDestino).
validacionColor(X,Y,"right",COrigen) :- tablero(celda(X+1,Y,_),ficha(CDestino,_)) & not mismoColor(COrigen,CDestino).
validacionColor(X,Y,"up",COrigen) :- tablero(celda(X,Y-1,_),ficha(CDestino,_)) & not mismoColor(COrigen,CDestino).
validacionColor(X,Y,"down",COrigen) :- tablero(celda(X,Y+1,_),ficha(CDestino,_)) & not mismoColor(COrigen,CDestino).
mismoColor(COrigen,CDestino) :- (COrigen=CDestino).

//Comprobacion completa de las condiciones de un movimiento correcto: Seleccion, tipo obstaculo
compObs(pos(X,Y),Dir):- validacionObs(X,Y,Dir).
validacionObs(X,Y,"left") :- tablero(celda(X-1,Y,_),Destino) & not tipoObstaculo(Destino).
validacionObs(X,Y,"right") :- tablero(celda(X+1,Y,_),Destino) & not tipoObstaculo(Destino).
validacionObs(X,Y,"up") :- tablero(celda(X,Y-1,_),Destino) & not tipoObstaculo(Destino).
validacionObs(X,Y,"down") :- tablero(celda(X,Y+1,_),Destino) & not tipoObstaculo(Destino).
tipoObstaculo(obstacle).
	
//Comprobacion completa de las condiciones de un movimiento correcto: Seleccion, tipo vacio
compVacio(pos(X,Y),Dir):- validacionVacio(X,Y,Dir).
validacionVacio(X,Y,"left") :- tablero(celda(X-1,Y,_),Destino) & not tipoVacio(Destino).
validacionVacio(X,Y,"right") :- tablero(celda(X+1,Y,_),Destino) & not tipoVacio(Destino).
validacionVacio(X,Y,"up") :- tablero(celda(X,Y-1,_),Destino) & not tipoVacio(Destino).
validacionVacio(X,Y,"down") :- tablero(celda(X,Y+1,_),Destino) & not tipoVacio(Destino).
tipoVacio(vacio).


	
validoMov(pos(X,Y),"up") :-
	enTablero(X,Y) &
	enTablero(X,Y-1).

validoMov(pos(X,Y),"down") :-
	enTablero(X,Y) &
	enTablero(X,Y+1).

validoMov(pos(X,Y),"left") :-
	enTablero(X,Y) &
	enTablero(X-1,Y).

validoMov(pos(X,Y),"right") :-
	enTablero(X,Y) &
	enTablero(X+1,Y).

enTablero(X,Y) :-
	size(N) & (X < N) & (Y < N) & (X >= 0) & (Y >= 0).

//obtencion del mejor movimiento

//comprobacion de cinco en T

compruebaT(X,Y):- plotioTAR(X,Y)|plotioTAB(X,Y)|plotioTD(X,Y)|plotioTI(X,Y).

plotioTAR(X,Y):- tableroVirtual(celda(X,Y,_),ficha(ColorO,_))& 
							tableroVirtual(celda(X-1,Y,_),ficha(ColorO,_))& 
							tableroVirtual(celda(X+1,Y,_),ficha(ColorO,_))&
							tableroVirtual(celda(X,Y-1,_),ficha(ColorO,_))& 
							tableroVirtual(celda(X,Y-2,_),ficha(ColorO,_)).

plotioTAB(X,Y):- tableroVirtual(celda(X,Y,_),ficha(ColorO,_))& 
							tableroVirtual(celda(X-1,Y,_),ficha(ColorO,_))& 
							tableroVirtual(celda(X+1,Y,_),ficha(ColorO,_))&
							tableroVirtual(celda(X,Y+1,_),ficha(ColorO,_))&
							tableroVirtual(celda(X,Y+2,_),ficha(ColorO,_)).

plotioTD(X,Y):- tableroVirtual(celda(X,Y,_),ficha(ColorO,_))& 
							 tableroVirtual(celda(X,Y-1,_),ficha(ColorO,_))& 
							 tableroVirtual(celda(X,Y+1,_),ficha(ColorO,_))&
							 tableroVirtual(celda(X+1,Y,_),ficha(ColorO,_))& 
							 tableroVirtual(celda(X+2,Y,_),ficha(ColorO,_)).

plotioTI(X,Y):- tableroVirtual(celda(X,Y,_),ficha(ColorO,_))& 
							 tableroVirtual(celda(X,Y-1,_),ficha(ColorO,_))& 
							 tableroVirtual(celda(X,Y+1,_),ficha(ColorO,_))&
							 tableroVirtual(celda(X-1,Y,_),ficha(ColorO,_))&
							 tableroVirtual(celda(X-2,Y,_),ficha(ColorO,_)).
						
//comprobacion de cinco en linea
comprueba5Linea(X,Y):- plotio5H(X,Y)| plotio5V(X,Y).	
						
						
plotio5H(X,Y):- tableroVirtual(celda(X,Y,_),ficha(ColorO,_))& 
							tableroVirtual(celda(X,Y-1,_),ficha(ColorO,_))& 
							tableroVirtual(celda(X,Y-2,_),ficha(ColorO,_))&
							tableroVirtual(celda(X+1,Y,_),ficha(ColorO,_))& 
							tableroVirtual(celda(X+2,Y,_),ficha(ColorO,_)).
							
plotio5V(X,Y):- tableroVirtual(celda(X,Y,_),ficha(ColorO,_))& 
							 tableroVirtual(celda(X,Y-1,_),ficha(ColorO,_))& 
							 tableroVirtual(celda(X,Y-2,_),ficha(ColorO,_))&
							 tableroVirtual(celda(X,Y+1,_),ficha(ColorO,_))& 
							 tableroVirtual(celda(X,Y+2,_),ficha(ColorO,_)).

							 
//comprobacion de cuatro en cubo							 
compruebaCubo(X,Y):- plotioCuboABI(X,Y)|plotioCuboABD(X,Y)|plotioCuboARI(X,Y)|plotioCuboARD(X,Y).


plotioCuboABI(X,Y):- tableroVirtual(celda(X,Y,_),ficha(ColorO,_))&
						tableroVirtual(celda(X-1,Y,_),ficha(ColorO,_))&
						tableroVirtual(celda(X-1,Y+1,_),ficha(ColorO,_))&
						tableroVirtual(celda(X,Y+1,_),ficha(ColorO,_)).

plotioCuboABD(X,Y):- tableroVirtual(celda(X,Y,_),ficha(ColorO,_))&
						tableroVirtual(celda(X+1,Y,_),ficha(ColorO,_))&
						tableroVirtual(celda(X+1,Y+1,_),ficha(ColorO,_))&
						tableroVirtual(celda(X,Y+1,_),ficha(ColorO,_)).

plotioCuboARI(X,Y):- tableroVirtual(celda(X,Y,_),ficha(ColorO,_))&
						tableroVirtual(celda(X-1,Y,_),ficha(ColorO,_))&
						tableroVirtual(celda(X-1,Y-1,_),ficha(ColorO,_))&
						tableroVirtual(celda(X,Y-1,_),ficha(ColorO,_)).

plotioCuboARD(X,Y):- tableroVirtual(celda(X,Y,_),ficha(ColorO,_))&
						tableroVirtual(celda(X+1,Y,_),ficha(ColorO,_))&
						tableroVirtual(celda(X+1,Y-1,_),ficha(ColorO,_))&
						tableroVirtual(celda(X,Y-1,_),ficha(ColorO,_)).



//comprobacion de cuatro en linea						
comprueba4Linea(X,Y,Dir):- plotio4I(X,Y)| plotio4D(X,Y)|plotio4AR(X,Y)| plotio4AB(X,Y).
							

plotio4I(X,Y):- tableroVirtual(celda(X,Y,_),ficha(ColorO,_))&
									tableroVirtual(celda(X-1,Y,_),ficha(ColorO,_))&
									tableroVirtual(celda(X-2,Y,_),ficha(ColorO,_))&
									tableroVirtual(celda(X+1,Y,_),ficha(ColorO,_)).
		
plotio4D(X,Y):- tableroVirtual(celda(X,Y,_),ficha(ColorO,_))&
									tableroVirtual(celda(X-1,Y,_),ficha(ColorO,_))&
									tableroVirtual(celda(X+1,Y,_),ficha(ColorO,_))&
									tableroVirtual(celda(X+2,Y,_),ficha(ColorO,_)).
							
					
plotio4AR(X,Y):- tableroVirtual(celda(X,Y,_),ficha(ColorO,_))&
									tableroVirtual(celda(X,Y-1,_),ficha(ColorO,_))&
									tableroVirtual(celda(X,Y-2,_),ficha(ColorO,_))&
									tableroVirtual(celda(X,Y+1,_),ficha(ColorO,_)).									
									
plotio4AB(X,Y):- tableroVirtual(celda(X,Y,_),ficha(ColorO,_))&
									tableroVirtual(celda(X,Y-1,_),ficha(ColorO,_))&
									tableroVirtual(celda(X,Y+1,_),ficha(ColorO,_))&
									tableroVirtual(celda(X,Y+2,_),ficha(ColorO,_)).
							

									
//comprobacion de tres en linea
compruebaTres(X,Y):- plotioUD(X,Y)|plotioLR(X,Y)|plotioL(X,Y)|plotioR(X,Y)|plotioU(X,Y)|plotioD(X,Y).
						
plotioUD (X,Y):- tableroVirtual(celda(X,Y,_),ficha(ColorO,_))&
							tableroVirtual(celda(X,Y-1,_),ficha(ColorO,_))&
							tableroVirtual(celda(X,Y+1,_),ficha(ColorO,_)). 	
	
plotioLR (X,Y):- tableroVirtual(celda(X,Y,_),ficha(ColorO,_))&
							tableroVirtual(celda(X-1,Y,_),ficha(ColorO,_))&
							tableroVirtual(celda(X+1,Y,_),ficha(ColorO,_)).
							
plotioL (X,Y):- tableroVirtual(celda(X,Y,_),ficha(ColorO,_))&
							tableroVirtual(celda(X-1,Y,_),ficha(ColorO,_))&
							tableroVirtual(celda(X-2,Y,_),ficha(ColorO,_)).
							
plotioR (X,Y):- tableroVirtual(celda(X,Y,_),ficha(ColorO,_))&
							tableroVirtual(celda(X+1,Y,_),ficha(ColorO,_))&
							tableroVirtual(celda(X+2,Y,_),ficha(ColorO,_)).
							
plotioU (X,Y):- tableroVirtual(celda(X,Y,_),ficha(ColorO,_))&
							tableroVirtual(celda(X,Y-1,_),ficha(ColorO,_))&
							tableroVirtual(celda(X,Y-2,_),ficha(ColorO,_)).
							
plotioD (X,Y):- tableroVirtual(celda(X,Y,_),ficha(ColorO,_))&
							tableroVirtual(celda(X,Y+1,_),ficha(ColorO,_))&
							tableroVirtual(celda(X,Y+2,_),ficha(ColorO,_)). 
	
							
//creacion aleatoria de un movimiento
obtenerpos(pos(X2,Y2)):- size(N)&
	.random(X12) &
	.random(Y12) &
	X2 = math.round(1000*X12)mod N &
	Y2 = math.round(1000*Y12)mod N .

creaDir(Dir) :-
	VDir = math.round(math.random(3)) &
	direccion(VDir,Dir).
	
direccion(0,"up").
direccion(1,"down").
direccion(2,"left").
direccion(3,"right").	

//obtencion de las coordenadas destiono del movimiento
nextMove(P1,P2,NX,NY,Dir):-
	(
	((Dir == "left") & (NX = (P1-1)) & (NY= P2) ) |
	((Dir == "right") & (NX = (P1+1)) & (NY = P2)) |
	((Dir == "down") & (NX = P1) & (NY = (P2+1))) |
	((Dir == "up") & (NX = P1) & (NY = (P2-1)))
	).


						
/* Initial goals */


/* Plans */



+valido[source(judge)] <- .print("Mi ultimo movimiento ha sido valido").

+puedesMover[source(judge)]<-
	.print("Acabo de recibir del judge el testigo de mover");
	!eligeMov.
						
						
+puedesMover[source(judge)]:true<- 
						.print("me he bloqueao").

+invalido(fueraTablero,Veces)[source(judge)] : Veces > 3 <-
	.print("Acabo de recibir del judge que he intentado mover fuera del tablero 4 veces");
	.print("Pierdo el turno.");
	.send(judge,tell,pasoTurno);
	.send(judge,untell,pasoTurno).					
						
+invalido(fueraTurno,Veces)[source(judge)] : Veces <3   <-
	.print("Acabo de recibir del judge que he intentado mover fuera de turno: ",  Veces, " veces.");
	!eligeMov.
	
+invalido(fueraTablero,Veces)[source(judge)] : Veces < 4 <-
.print("Acabo de recibir del judge que he intentado mover fuera del tablero: ",  Veces, " veces.");
	!eligeMov.
	
+tryAgain[source(judge)]<- 
	.print("El judge me pide otro movimiento.......................................");
	 !eligeMov.

	
+!eligeMov<- !actuVirtual;
			-+puntos(0);
			!agrupacionRandom;
			.findall(pos(Xx,Yy),(tableroVirtual(celda(Xx,Yy,_),Ficha) & not(Ficha==obstacle) & not(Ficha==vacio)),ResulVirtual);
			for(.member(pos(X,Y),ResulVirtual)){
				/*if(Y mod 2==0){
					for(.member(Dir,["left","right"])){	
					if(movimientoValido(pos(X,Y),Dir)){
						?nextMove(X,Y,NX,NY,Dir);
						-tableroVirtual(celda(X,Y,Du),FichaA);
						-tableroVirtual(celda(NX,NY,Du2),FichaB);
						+tableroVirtual(celda(X,Y,Du),FichaB);
						+tableroVirtual(celda(NX,NY,Du2),FichaA);
						!agrupacion(X,Y,Dir);
						!actuVirtual;
					};
				};
				}else{  */
					for(.member(Dir,["up","down","left","right"])){	
						if(movimientoValido(pos(X,Y),Dir)){
							?nextMove(X,Y,NX,NY,Dir);
							-tableroVirtual(celda(X,Y,Du),FichaA);
							-tableroVirtual(celda(NX,NY,Du2),FichaB);
							+tableroVirtual(celda(X,Y,Du),FichaB);
							+tableroVirtual(celda(NX,NY,Du2),FichaA);
							!agrupacion(X,Y,Dir);
							!actuVirtual;
						};
					};
				//};
			};
			!seleccionEstrategia.
			
			

			
			
			
	
//ESTRATEGIA NIVELES 1 Y 2			
//////////////////////////////////////////////////			
+!seleccionEstrategia:(level(L)& (L=1|L=2))&mejorMovPuntos(0,0,"right",0)<-//si no hay agrupacion que de puntos,mov random
			?movRandom(Xenv,Yenv,Direnv,_);
			.print("Envio el movimiento aleatorio",Xenv," ",Yenv," ",Direnv,", no hay agrupaciones");
			.send(judge,tell,moverDesdeEnDireccion(pos(Xenv,Yenv),Direnv));
			-mejorMovPuntos(_,_,_,_);
			+mejorMovPuntos(0,0,"right",0).
			
+!seleccionEstrategia:(level(L)& (L=1|L=2))&mejorMovPuntos(Xenv,Yenv,Direnv,_)<-
			.print("Envio el movimiento ",Xenv," ",Yenv," ",Direnv," para obtener maximo de puntos");
			.send(judge,tell,moverDesdeEnDireccion(pos(Xenv,Yenv),Direnv));
			-mejorMovPuntos(_,_,_,_);
			+mejorMovPuntos(0,0,"right",0).
			
/////////////////////////////////////////////////////			

+!seleccionEstrategia<-.print("Algo malo ha pasado con la estrategia").





+!actuVirtual<-.findall(tableroVirtual(X,Y),tableroVirtual(X,Y),Lista);
			for ( .member(Estructure,Lista) ) {
				.abolish(Estructure);
			};
			.findall(tablero(Celda,Ficha),tablero(Celda,Ficha),Resul);
			for(.member(tablero(Celda,Ficha),Resul)){
				+tableroVirtual(Celda,Ficha);
			}.

+!agrupacion(X,Y,Dir):plotioTAR(X,Y)<- -+calculaPuntos(X,Y);
										-+calculaPuntos(X,Y-1);
										-+calculaPuntos(X,Y-2);
										-+calculaPuntos(X-1,Y);
										-+calculaPuntos(X+1,Y);
										
							
										
										-+guardadoMovimiento(X,Y,Dir);
										-+puntos(0).
													

													
+!agrupacion(X,Y,Dir):plotioTAB(X,Y)<- -+calculaPuntos(X,Y);
										-+calculaPuntos(X,Y+1);
										-+calculaPuntos(X,Y+2);
										-+calculaPuntos(X-1,Y);
										-+calculaPuntos(X+1,Y);
								
										-+guardadoMovimiento(X,Y,Dir);
										-+puntos(0).
											
											
+!agrupacion(X,Y,Dir):plotioTD(X,Y)<- -+calculaPuntos(X,Y);
										-+calculaPuntos(X,Y-1);
										-+calculaPuntos(X,Y+1);
										-+calculaPuntos(X+1,Y);
										-+calculaPuntos(X+2,Y);
										
						
										-+guardadoMovimiento(X,Y,Dir);
										-+puntos(0).
										
+!agrupacion(X,Y,Dir):plotioTI(X,Y)<- -+calculaPuntos(X,Y);
										-+calculaPuntos(X,Y-1);
										-+calculaPuntos(X,Y+1);
										-+calculaPuntos(X-1,Y);
										-+calculaPuntos(X-2,Y);
							
										-+guardadoMovimiento(X,Y,Dir);
										-+puntos(0).
										
+!agrupacion(X,Y,Dir):plotio5H(X,Y)<- -+calculaPuntos(X,Y);
										-+calculaPuntos(X-1,Y);
										-+calculaPuntos(X-2,Y);
										-+calculaPuntos(X+1,Y);
										-+calculaPuntos(X+2,Y);
										
							
										-+guardadoMovimiento(X,Y,Dir);
										-+puntos(0).
										
+!agrupacion(X,Y,Dir):plotio5V(X,Y)<- -+calculaPuntos(X,Y);
										-+calculaPuntos(X,Y-1);
										-+calculaPuntos(X,Y-2);
										-+calculaPuntos(X,Y+1);
										-+calculaPuntos(X,Y+2);
										
							
										
										-+guardadoMovimiento(X,Y,Dir);
										-+puntos(0).
													
													
+!agrupacion(X,Y,Dir):plotioCuboABI(X,Y)<--+calculaPuntos(X,Y);
										-+calculaPuntos(X-1,Y);
										-+calculaPuntos(X-1,Y+1);
										-+calculaPuntos(X,Y+1);
										
								
										
										-+guardadoMovimiento(X,Y,Dir);
										-+puntos(0).
										
+!agrupacion(X,Y,Dir):plotioCuboARI(X,Y)<- -+calculaPuntos(X,Y);
										-+calculaPuntos(X-1,Y);
										-+calculaPuntos(X-1,Y-1);
										-+calculaPuntos(X,Y-1);
							
										
										-+guardadoMovimiento(X,Y,Dir);
										-+puntos(0).
										
+!agrupacion(X,Y,Dir):plotioCuboABD(X,Y)<- -+calculaPuntos(X,Y);
										-+calculaPuntos(X+1,Y);
										-+calculaPuntos(X+1,Y+1);
										-+calculaPuntos(X,Y+1);
										
							
										-+guardadoMovimiento(X,Y,Dir);
										
										-+puntos(0).
										
+!agrupacion(X,Y,Dir):plotioCuboARD(X,Y)<- -+calculaPuntos(X,Y);
										-+calculaPuntos(X+1,Y);
										-+calculaPuntos(X+1,Y-1);
										-+calculaPuntos(X,Y-1);
										
								
										
										-+guardadoMovimiento(X,Y,Dir);
										-+puntos(0).
										
+!agrupacion(X,Y,Dir):plotio4AR(X,Y)<- -+calculaPuntos(X,Y);
										-+calculaPuntos(X,Y-1);
										-+calculaPuntos(X,Y-2);
										-+calculaPuntos(X,Y+1);
								
										
										-+guardadoMovimiento(X,Y,Dir);
										-+puntos(0).
										
+!agrupacion(X,Y,Dir):plotio4AB(X,Y)<- -+calculaPuntos(X,Y);
										-+calculaPuntos(X,Y-1);
										-+calculaPuntos(X,Y+1);
										-+calculaPuntos(X,Y+2);
										
							
										
										-+guardadoMovimiento(X,Y,Dir);
										-+puntos(0).
										
+!agrupacion(X,Y,Dir):plotio4I(X,Y)<- -+calculaPuntos(X,Y);
										-+calculaPuntos(X+1,Y);
										-+calculaPuntos(X-1,Y);
										-+calculaPuntos(X-2,Y);
										
									
										-+guardadoMovimiento(X,Y,Dir);
										-+puntos(0).
										
+!agrupacion(X,Y,Dir):plotio4D(X,Y)<- -+calculaPuntos(X,Y);
										-+calculaPuntos(X-1,Y);
										-+calculaPuntos(X+1,Y);
										-+calculaPuntos(X+2,Y);
										
							
										-+guardadoMovimiento(X,Y,Dir);
										-+puntos(0).
													
+!agrupacion(X,Y,Dir):plotioUD(X,Y)<- -+calculaPuntos(X,Y);
										-+calculaPuntos(X,Y-1);
										-+calculaPuntos(X,Y+1);
										
									
										-+guardadoMovimiento(X,Y,Dir);
										-+puntos(0).

+!agrupacion(X,Y,Dir):plotioLR(X,Y)<- -+calculaPuntos(X,Y);
										-+calculaPuntos(X-1,Y);
										-+calculaPuntos(X+1,Y);
										
									
										-+guardadoMovimiento(X,Y,Dir);
										-+puntos(0).
										
+!agrupacion(X,Y,Dir):plotioL(X,Y)<- -+calculaPuntos(X,Y);
										-+calculaPuntos(X-1,Y);
										-+calculaPuntos(X-2,Y);
										
									
										-+guardadoMovimiento(X,Y,Dir);
										-+puntos(0).
										
+!agrupacion(X,Y,Dir):plotioR(X,Y)<- -+calculaPuntos(X,Y);
										-+calculaPuntos(X+1,Y);
										-+calculaPuntos(X+2,Y);
										
								
										
										-+guardadoMovimiento(X,Y,Dir);
										
										-+puntos(0).
										
+!agrupacion(X,Y,Dir):plotioD(X,Y)<- -+calculaPuntos(X,Y);
										-+calculaPuntos(X,Y+1);
										-+calculaPuntos(X,Y+2);
								
										-+guardadoMovimiento(X,Y,Dir);
										-+puntos(0)
										.
										
+!agrupacion(X,Y,Dir):plotioU(X,Y)<- -+calculaPuntos(X,Y);
										-+calculaPuntos(X,Y-1);
										-+calculaPuntos(X,Y-2);
										
										-+guardadoMovimiento(X,Y,Dir);
										-+puntos(0);
										.
										
+!agrupacion(X,Y,Dir).

+!agrupacionRandom:obtenerpos(pos(Xrandom,Yrandom))&creaDir(Dirrandom)<-	
							-+movRandom(Xrandom,Yrandom,Dirrandom,0).

							
							
+calculaPuntos(X,Y):tableroVirtual(celda(X,Y,_),ficha(_,TipoFicha))<- 
								if (TipoFicha==in) {
								-puntos(Puntos);
								+puntos(Puntos+1);
								};
								
								if (TipoFicha==ip) {
								-puntos(Puntos);
								+puntos(Puntos+2);
								};
								if (TipoFicha==gs) {
								-puntos(Puntos);
								+puntos(Puntos+4);
								};
								if (TipoFicha==co) {
								-puntos(Puntos);
								+puntos(Puntos+6);
								};
								if (TipoFicha==ct) {
								-puntos(Puntos);
								+puntos(Puntos+8);
								}.
								
+guardadoMovimiento(X,Y,Dir)<-
					?mejorMovPuntos(Xmejor,Ymejor,DirMejor,PuntosMejor);
					?puntos(PuntosActual);
					
						if(PuntosActual>PuntosMejor){
							-mejorMovPuntos(Xmejor,Ymejor,DirMejor,PuntosMejor);
							+mejorMovPuntos(X,Y,Dir,PuntosActual);
							
						};
						if(PuntosActual==PuntosMejor){
							if(Y>Ymejor){
								-mejorMovPuntos(Xmejor,Ymejor,DirMejor,PuntosMejor);
								+mejorMovPuntos(X,Y,Dir,PuntosActual);
							}
						}
						.	
							
+borraTablero[source(judge)]<- 
-borraTablero[source(judge)];
.findall(tablero(X,Y),tablero(X,Y),Lista);
for ( .member(Estructure,Lista) ) {
			.abolish(Estructure);
		 };
!actuVirtual.	
