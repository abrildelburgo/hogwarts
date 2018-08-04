%HOGWARTS

mago(harry, mestiza, [coraje, amistad, orgullo, inteligencia]).
mago(ron, pura, [amistad, diversion, coraje]).
mago(hermione, impura, [inteligencia, coraje, responsabilidad, amistad, orgullo]).
mago(hannahAbbott, mestiza, [amistad, diversion]).
mago(draco, pura, [inteligencia, orgullo]).
mago(lunaLovegood, mestiza, [inteligencia, responsabilidad, amistad, coraje]).

odia(harry,slytherin).
odia(draco,hufflepuff).

casa(gryffindor).
casa(hufflepuff).
casa(ravenclaw).
casa(slytherin).

caracteriza(gryffindor,amistad).
caracteriza(gryffindor,coraje).
caracteriza(slytherin,orgullo).
caracteriza(slytherin,inteligencia).
caracteriza(ravenclaw,inteligencia).
caracteriza(ravenclaw,responsabilidad).
caracteriza(hufflepuff,amistad).
caracteriza(hufflepuff,diversion).

lugarProhibido(bosque,50).
lugarProhibido(seccionRestringida,10).
lugarProhibido(tercerPiso,75).

alumnoFavorito(flitwick, hermione).
alumnoFavorito(snape, draco).
alumnoOdiado(snape, harry).

hizo(ron, buenaAccion(jugarAlAjedrez, 50)).
hizo(harry, fueraDeCama).
hizo(hermione, irA(tercerPiso)).
hizo(hermione, responder(“Donde se encuentra un Bezoar”, 15, snape)).
hizo(hermione, responder(“Wingardium Leviosa”, 25, flitwick)).
hizo(ron, irA(bosque)).
hizo(draco, irA(mazmorras)).


%PUNTO1
permiteEntrar(slytherin,Mago):-
	mago(Mago,Sangre,_),
	Sangre\=impura.
permiteEntrar(Casa,Mago):-
	mago(Mago,_,_),
	casa(Casa),
	Casa\=slytherin.

%PUNTO2
tieneCaracter(Mago,Casa):-
	mago(Mago,_,Lista),
	casa(Casa),
	forall(caracteriza(Casa,Caracteristica),member(Caracteristica,Lista)).

%PUNTO3
casaPosible(Mago,Casa):-
	permiteEntrar(Casa,Mago),
	tieneCaracter(Mago,Casa),
	not(odia(Mago,Casa)).

%PUNTO4
cadenaDeAmistades(ListaMagos):-
	todosAmistosos(ListaMagos),
	todosMismaCasa(ListaMagos).

todosAmistosos(ListaMagos):-
	forall(member(Mago,ListaMagos),esAmistoso(Mago)).

esAmistoso(Mago):-
	mago(Mago,_,Lista),
	member(amistad,Lista).

todosMismaCasa(ListaMagos):-
	nth0(0,ListaMagos,PrimerMago),
	permiteEntrar(Casa,PrimerMago),
	forall(member(Mago,ListaMagos),permiteEntrar(Casa,Mago)).

%LA COSA DE LAS CASAS
%PUNTO5

esBuenAlumno(Mago):-
	hizo(Mago,_),
	forall(hizo(Mago,Accion),not((tipoAccion(Mago,Accion,Puntaje),Puntaje<0))).

%PUNTO6
puntosDeCasa(Casa,PuntajeTotal):-
	casa(Casa),
	findall(Puntaje,obtenerPuntaje(Casa,Puntaje),ListaPuntajes),
	sumlist(ListaPuntajes,PuntajeTotal).

obtenerPuntaje(Casa,Puntaje):-
	esDe(Mago,Casa),
	hizo(Mago,Accion),
	findall(PuntajeParcial,tipoAccion(Mago,Accion,PuntajeParcial),Lista),
	sumlist(Lista,Puntaje).

tipoAccion(_,buenaAccion(_,Puntaje),Puntaje).
tipoAccion(Mago,responder(_,Puntaje,Profesor),PuntajeFinal):-
	alumnoFavorito(Profesor,Mago),
	PuntajeFinal is Puntaje*2.
tipoAccion(Mago,responder(_,Puntaje,Profesor),0):-
	alumnoOdiado(Profesor,Mago).
tipoAccion(Mago,responder(_,Puntaje,Profesor),Puntaje):-
	mago(Mago,_,_),
	not(alumnoOdiado(Profesor,Mago)),
	not(alumnoFavorito(Profesor,Mago)).
tipoAccion(_,irA(Lugar),PuntajeFinal):-
	lugarProhibido(Lugar,Puntaje),
	PuntajeFinal is Puntaje*(-1).
tipoAccion(_,irALugar(Lugar),0):-
	not(lugarProhibido(Lugar,_)).
tipoAccion(_,fuerraDeLaCama,-50).

%PUNTO7
casaGanadora(Casa):-
	puntosDeCasa(Casa,Puntaje),	
	forall((puntosDeCasa(Casa2,Puntaje2),Casa2\=Casa),Puntaje>Puntaje2).