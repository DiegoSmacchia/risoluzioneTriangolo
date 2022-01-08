/* Programma Prolog per la risoluzione di un triangolo nel caso in cui una di queste ipotesi sia vera:
    - conosciamo due angoli e il lato compreso tra loro;
    - conosciamo due angoli e un lato non compreso tra loro;
    - conosciamo due lati e l'angolo compreso tra loro;
    - conosciamo due lati e un angolo non compreso tra loro;
    - conosciamo i tre lati. */
main :- 
    sceltaCaso(Scelta),
    (Scelta > 0 -> 
    eseguiScelta(Scelta), 
    main;
    write('Uscita.'), nl).

/* Il predicato sceltaCaso stampa un semplice menu per la scelta del caso che si desidera trattare,
    una volta che l'utente ha inserito il valore corrispondente al caso voluto lo valida,
    ripetendo la richiesta in caso di input non valido, cioè qualunque valore diverso da un intero
    compreso tra 0 e 5. */
sceltaCaso(Scelta) :- 
    write('-----------------------------------------------------------------'), nl,
    write('Scegliere uno dei seguenti casi:'), nl,
    write('1) Sono noti due angoli ed il lato tra essi compreso.'), nl,
    write('2) Sono noti due angoli ed un lato NON compreso tra loro.'), nl,
    write('3) Sono noti due lati e l\'angolo tra essi compreso.'), nl,
    write('4) Sono noti due lati ed un angolo NON compreso tra loro.'), nl,
    write('5) Sono noti i tre lati.'), nl,
    write('0) Uscita.'), nl,
    write('-----------------------------------------------------------------'), nl,

    read(Scelta),
    verificaSceltaCaso(Scelta) ; write('Valore inserito non valido. Riprovare.'), nl, sceltaCaso(Scelta).
    
/* Il predicato verificaSceltaCaso prende in ingresso il valore inserito dall'utente quando seleziona uno dei casi possibili,
    e si assicura che il valore inserito sia un intero compreso nell'intervallo aperto (-1, 6) tramite il predicato verificaValoreCompreso. */
verificaSceltaCaso(CasoScelto) :- 
    integer(CasoScelto), 
    verificaValoreCompreso(CasoScelto, -1, 6).

/* Il predicato verificaValoreCompreso controlla se un dato valore appartiene o meno ad un 
    determinato intervallo aperto, prende in ingresso tre argomenti:
    - il primo è il valore da verificare;
    - il secondo è il valore minimo dell'intervallo aperto;
    - il terzo è il valore massimo dell'intervallo aperto. */
verificaValoreCompreso(ValoreDaVerificare, Minimo, Massimo) :- 
    ValoreDaVerificare > Minimo, 
    ValoreDaVerificare < Massimo.

/* Il predicato eseguiScelta prende in ingresso un numero intero, e richiama il predicato
    scelto dall'utente per risolvere il triangolo.
    l'ultima linea, che stampa un messaggio di errore, non sarebbe necessario dal momento che
    il predicato sceltaCaso si è già occupato della validazione della scelta, ma è stato
    inserito comunque per definire il predicato per ogni valore possibile. */
eseguiScelta(Scelta) :-
    (Scelta =:= 1 -> risolviDueAngoliLatoCompreso, nl;
    Scelta =:= 2 -> risolviDueAngoliLatoNonCompreso, nl;
    Scelta =:= 3 -> risolviDueLatiAngoloCompreso, nl;
    Scelta =:= 4 -> risolviDueLatiAngoloNonCompreso, nl;
    Scelta =:= 5 -> risolviTreLati, nl;
    write('Scelta non valida.'), nl).

/* Il predicato risolviDueAngoliLatoCompreso richiede all'utente l'inserimento del valore (in gradi) dell'ampiezza di due angoli,
    alpha e beta, e della lunghezza del lato compreso tra loro, AB, tramite il predicato richiestaDatoTriangolo.
    Dopo averli ottenuti calcola per differenza l'angolo gamma, poi tramite il teorema dei seni
    calcola i due lati mancanti. 
    Una volta ottenuti tutti i dati richiama il predicato stampaTriangolo.
    NOTA: l'angolo alpha può assumere tutti i valori compresi nell'intervallo aperto (0, 180), mentre
        beta dovrà appartenere all'intervallo aperto (0, (180 - Alpha)) poiché la somma degli angoli interni di un triangolo è sempre uguale a 180. */
risolviDueAngoliLatoCompreso :-
    richiestaDatoTriangolo('angolo alpha', 0, 180, Alpha),
    MassimoBeta is 180 - Alpha,
    richiestaDatoTriangolo('angolo beta', 0, MassimoBeta, Beta),
    richiestaDatoTriangolo('lato AB', 0, 774352305.874, LatoAB),

    Gamma is 180 - (Alpha + Beta),
    ottieniLatoTeoremaSeni(LatoAB, Gamma, Alpha, LatoBC),
    ottieniLatoTeoremaSeni(LatoAB, Gamma, Beta, LatoAC),

    stampaTriangolo(LatoAB, LatoBC, LatoAC, Alpha, Beta, Gamma).

/* Il predicato risolviDueAngoliLatoNonCompreso richiede all'utente l'inserimento del valore (in gradi) dell'ampiezza di due angoli,
    alpha e beta, e della lunghezza di un Lato NON compreso tra loro, che chiamiamo BC, tramite 
    il predicato richiestaDatoTriangolo.
    Dopo averli ottenuti calcola per differenza l'angolo gamma, poi tramite il teorema dei seni
    calcola i due lati mancanti. 
    Una volta ottenuti tutti i dati richiama il predicato stampaTriangolo.
    NOTA: l'angolo alpha può assumere tutti i valori compresi nell'intervallo aperto (0, 180), mentre
        beta dovrà appartenere all'intervallo aperto (0, (180 - Alpha)) poiché la somma degli angoli interni di un triangolo è sempre uguale a 180. */
risolviDueAngoliLatoNonCompreso :-
    richiestaDatoTriangolo('angolo alpha', 0, 180, Alpha),
    MassimoBeta is 180 - Alpha,
    richiestaDatoTriangolo('angolo beta', 0, MassimoBeta, Beta),
    richiestaDatoTriangolo('Lato BC', 0, 774352305.874, LatoBC),

    Gamma is 180 - (Alpha + Beta),
    ottieniLatoTeoremaSeni(LatoBC, Alpha, Gamma, LatoAB),
    ottieniLatoTeoremaSeni(LatoAB, Gamma, Beta, LatoAC),

    stampaTriangolo(LatoAB, LatoBC, LatoAC, Alpha, Beta, Gamma).

/* Il predicato risolviDueLatiAngoloCompreso richiede all'utente l'inserimento della lunghezza di due lati, AB e BC, e del valore 
    (in gradi) dell'ampiezza dell'angolo compreso tra loro, beta, tramite il predicato richiestaDatoTriangolo.
    Dopo averli ottenuti calcola la dimensione del lato mancante tramite il teorema di Carnot,
    l'angolo gamma tramite il teorema dei seni e alpha per differenza.
    Una volta ottenuti tutti i dati richiama il predicato stampaTriangolo. 
    NOTA: La dimensione massima di ogni lato è stata fissata a 774352305.874, per evitare di superare il valore 999999999999999999 durante il calcolo
        del perimetro, causando un errore di overflow qualora i lati fossero sufficientemente grandi.*/
risolviDueLatiAngoloCompreso :-
    richiestaDatoTriangolo('Lato AB', 0, 774352305.874, LatoAB),
    richiestaDatoTriangolo('Lato BC', 0, 774352305.874, LatoBC),
    richiestaDatoTriangolo('angolo beta', 0, 180, Beta),

    ottieniLatoTeoremaCarnot(LatoAB, LatoBC, Beta, LatoAC),
    ottieniAngoloTeoremaSeni(LatoAC, LatoAB, Beta, Gamma),
    Alpha is 180 - (Beta + Gamma),

    stampaTriangolo(LatoAB, LatoBC, LatoAC, Alpha, Beta, Gamma).

/* Il predicato risolviDueLatiAngoloNonCompreso richiede all'utente l'inserimento della lunghezza di due lati, AB e BC, e del valore 
    (in gradi) dell'ampiezza di un angolo NON compreso tra loro, che chiamiamo alpha, tramite il predicato richiestaDatoTriangolo.
    Dopo averli ottenuti calcola il valore dell'ampiezza dell'angolo gamma tramite il teorema dei seni, beta per differenza
    e la dimensione del lato mancante tramite il teorema di Carnot.
    Una volta ottenuti tutti i dati richiama il predicato stampaTriangolo. 
    NOTA: La dimensione massima di ogni lato è stata fissata a 774352305.874, per evitare di superare il valore 999999999999999999 durante il calcolo
        del perimetro, causando un errore di overflow. (qualora i lati fossero sufficientemente grandi) */
risolviDueLatiAngoloNonCompreso :-
    richiestaDatoTriangolo('Lato AB', 0, 774352305.874, LatoAB),
    richiestaDatoTriangolo('Lato BC', 0, 774352305.874, LatoBC),
    richiestaDatoTriangolo('angolo alpha', 0, 180, Alpha),

    ottieniAngoloTeoremaSeni(LatoBC, LatoAB, Alpha, Gamma),
    Beta is 180 - (Alpha + Gamma),
    ottieniLatoTeoremaCarnot(LatoAB, LatoBC, Beta, LatoAC),

    stampaTriangolo(LatoAB, LatoBC, LatoAC, Alpha, Beta, Gamma).

/* Il predicato risolviTreLati richiede all'utente l'inserimento della lunghezza dei tre lati, AB, BC e AC
    tramite il predicato richiestaDatoTriangolo.
    Dopo averli ottenuti calcola alpha tramite il teorema di Carnot, beta tramite il teorema dei seni
    e gamma per differenza.
    Una volta ottenuti tutti i dati richiama il predicato stampaTriangolo.
    NOTA: I lati AB e BC possono assumere tutti i valori compresi nell'intervallo aperto (0, 774352305.874), mentre
        AC dovrà appartenere all'intervallo aperto ( |LatoAB - LatoAC| ) , (latoAB + latoBC) ) 
        poiché ogni lato deve essere di lunghezza inferiore alla somma degli altri due. */
risolviTreLati :-
    richiestaDatoTriangolo('Lato AB', 0, 774352305.874, LatoAB),
    richiestaDatoTriangolo('Lato BC', 0, 774352305.874, LatoBC),
    LatoMinimo is abs(LatoAB - LatoBC),
    LatoMassimo is LatoAB + LatoBC,
    richiestaDatoTriangolo('Lato AC', LatoMinimo, LatoMassimo, LatoAC),
    ottieniAngoloTeoremaCarnot(LatoAC, LatoAB, LatoBC, Alpha),
    ottieniAngoloTeoremaSeni(LatoBC, LatoAC, Alpha, Beta),
    Gamma is 180 - (Alpha + Beta),

    stampaTriangolo(LatoAB, LatoBC, LatoAC, Alpha, Beta, Gamma).

/* Il predicato richiestaDatoTriangolo richiede all'utente l'inserimento del valore di un dato 
    del triangolo e ne verifica la validità:
    - il primo argomento è il nome del dato che vogliamo ottenere;
    - il secondo argomento indica il valore minimo dell'intervallo aperto di valori che il dato può assumere;
    - il terzo argomento indica il valore massimo dell'intervallo aperto di valori che il dato può assumere;
    - il quarto argomento è il dato del triangolo. 
    Quando l'utente inserisce il valore richiesto si verifica che sia un numero decimale, e anche che sia compreso 
    nell'intervallo aperto (Minimo, Massimo) tramite il predicato verificaValoreCompreso. */
richiestaDatoTriangolo(DatoRichiesto, Minimo, Massimo, DatoTriangolo) :-
    write('Inserire il valore di \''),
    write(DatoRichiesto),
    write('\' (maggiore di '),
    write(Minimo),
    write(' e minore di '),
    write(Massimo),
    write('): '),
    read(DatoTriangolo),
    number(DatoTriangolo),
    verificaValoreCompreso(DatoTriangolo, Minimo, Massimo) ;
    write('Valore inserito non valido. Riprovare.'), nl,
    richiestaDatoTriangolo(DatoRichiesto, Minimo, Massimo, DatoTriangolo).

/* Il predicato ottieniLatoTeoremaSeni calcola la lunghezza di un lato del triangolo:
    - il primo argomento è la lunghezza di un lato conosciuto;
    - il secondo argomento è la dimensione in gradi dell'angolo opposto al lato conosciuto; 
    - il terzo argomento è la dimensione in gradi di un altro angolo (l'angolo opposto al lato di cui vogliamo calcolare la lunghezza);
    - il quarto argomento è il lato da calcolare.
    NOTA: La funzione aritmetica sin prende in ingresso la dimensione degli angoli in radianti, quindi è necessario effettuare una conversione
        tramite il predicato convertiGradiInRadianti. */
ottieniLatoTeoremaSeni(LatoConosciuto, AngoloOpposto, AngoloNonOpposto, LatoIgnoto) :-
    convertiGradiInRadianti(AngoloOpposto, AngoloOppostoRadianti),
    convertiGradiInRadianti(AngoloNonOpposto, AngoloNonOppostoRadianti),
    LatoIgnoto is LatoConosciuto / sin(AngoloOppostoRadianti) * sin(AngoloNonOppostoRadianti).

/* Il predicato ottieniAngoloTeoremaSeni calcola l'ampiezza in gradi di un angolo del triangolo:
    - il primo argomento è la lunghezza di uno dei lati conosciuti;
    - il secondo argomento è la lunghezza del lato opposto all'angolo da calcolare;
    - il terzo argomento è la dimensione in gradi di un angolo (l'angolo opposto al lato passato come primo argomento);
    - il quarto argomento è l'angolo da calcolare.
    NOTE: 
    - la funzione aritmetica sin prende in ingresso la dimensione degli angoli in radianti, quindi è necessario effettuare una conversione
        tramite il predicato convertiGradiInRadianti;
    - la funzione aritmetica asin calcola la dimensione dell'angolo in radianti, quindi è necessario effettuare una conversione
        tramite il predicato convertiRadiantiInGradi. */
ottieniAngoloTeoremaSeni(PrimoLato, SecondoLato, AngoloConosciuto, AngoloIgnoto) :-
    convertiGradiInRadianti(AngoloConosciuto, AngoloConosciutoRadianti),
    AngoloIgnotoRadianti is asin( sin( AngoloConosciutoRadianti) / PrimoLato*SecondoLato ),
    convertiRadiantiInGradi(AngoloIgnotoRadianti, AngoloIgnoto).

/* Il predicato ottieniLatoTeoremaCarnot calcola la lunghezza di un lato del triangolo:
    - il primo e il secondo argomento sono le dimensioni degli altri due lati;
    - il terzo è argomento la dimensione in gradi dell'angolo tra loro compreso;
    - il quarto è il lato da calcolare.
    NOTA: La funzione aritmetica cos prende in ingresso la dimensione degli angoli in radianti, quindi è necessario effettuare una conversione
        tramite il predicato convertiGradiInRadianti. */
ottieniLatoTeoremaCarnot(PrimoLato, SecondoLato, AngoloCompreso, LatoIgnoto) :-
    convertiGradiInRadianti(AngoloCompreso, AngoloCompresoRadianti),
    LatoIgnoto is (PrimoLato^2 + SecondoLato^2 - 2*PrimoLato*SecondoLato * cos(AngoloCompresoRadianti))**0.5.

/* Il predicato ottieniAngoloTeoremaCarnot calcola la dimensione di un angolo del triangolo:
    - il primo e il secondo argomento sono la lunghezza dei due lati NON opposti all'angolo da calcolare;
    - il terzo argomento è la lunghezza del lato opposto all'angolo da calcolare;
    - il quarto argomento è il lato da calcolare.
    NOTE:
    - la funzione aritmetica cos prende in ingresso la dimensione degli angoli in radianti, quindi è necessario effettuare una conversione
        tramite il predicato convertiGradiInRadianti.
    - la funzione acos calcola la dimensione dell'angolo in radianti, quindi è necessario effettuare una conversione
        tramite il predicato convertiRadiantiInGradi. */
ottieniAngoloTeoremaCarnot(PrimoLatoNonOpposto, SecondoLatoNonOpposto, LatoOpposto, AngoloIgnoto) :-
    AngoloIgnotoRadianti is acos( (PrimoLatoNonOpposto^2 + SecondoLatoNonOpposto^2 - LatoOpposto^2) / (2 * PrimoLatoNonOpposto * SecondoLatoNonOpposto)),
    convertiRadiantiInGradi(AngoloIgnotoRadianti, AngoloIgnoto).


/* Il predicato stampaTriangolo effettua la stampa del valore dei lati, degli angoli, del perimetro e dell'area del triangolo
    prendendo in ingresso sei argomenti:
    - i primi tre sono rispettivamente i lati AB, BC e AC;
    - gli ultimi tre sono rispettivamente gli angoli alpha, beta e gamma. */
stampaTriangolo(LatoAB, LatoBC, LatoAC, Alpha, Beta, Gamma) :-
    Perimetro is LatoAB + LatoBC + LatoAC,
    convertiGradiInRadianti(Alpha, AlphaRadianti),
    Area is (LatoAB * LatoAC * sin(AlphaRadianti))/2,
    write('-----------------------------------------------------------------'), nl,
    write('I dati del triangolo risolto sono i seguenti:'), nl,
    write('Lato AB =        '),
    write(LatoAB), nl,
    write('Lato BC =        '),
    write(LatoBC), nl,
    write('Lato AC =        '),
    write(LatoAC), nl,
    write('Angolo alpha =   '),
    write(Alpha), nl,
    write('Angolo beta =    '),
    write(Beta), nl,
    write('Angolo gamma =   '),
    write(Gamma), nl,
    write('Perimetro =      '),
    write(Perimetro), nl,
    write('Area =           '),
    write(Area), nl,
    write('-----------------------------------------------------------------'), nl.

/* Il predicato convertiGradiInRadianti converte il valore dell'ampiezza di un angolo da gradi a radianti. */
convertiGradiInRadianti(AngoloGradi, AngoloRadianti) :-
    AngoloRadianti is AngoloGradi * pi / 180.

/* Il predicato convertiRadiantiInGradi converte il valore dell'ampiezza di un angolo da radianti a gradi. */
convertiRadiantiInGradi(AngoloRadianti, AngoloGradi) :-
    AngoloGradi is AngoloRadianti * 180/pi.