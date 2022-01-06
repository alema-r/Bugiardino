:- [farmaci].
:- [parole_chiave].
:- dynamic frase_rilevante/2.
:- retractall(frase_rilevante(_,_)).
%	carica_frasi(?NomeFarmaco, ?FoglioIllustrativo) (meta-predicato)
%	lancia `dividi_frasi` con `NomeFarmaco` e `FoglioIllustrativo` come argomenti
carica_frasi() :-
	farmaco(NomeFarmaco,_,_,FoglioIllustrativo),
	dividi_frasi(FoglioIllustrativo, [], [], NomeFarmaco).

%	trova tutte le soluzioni di `carica_frasi()` attraverso `findall`
%	estrae tutte le frasi rilevanti dai fogli illustrativi
inizializza() :- findall(_,carica_frasi(),_).

%	fine_frase(+P)
%	controlla se P è l'ultima parola della frase, cioè se contiene un punto
fine_frase(P) :-
    sub_atom(P,_,1,0,'.').

%	aggiungi_lista(+Lista, +ListadiListe, ?NuovaListadiListe) 
%	NuovaListadiListe è la concatenzatione di Lista e ListadiListe 
aggiungi_lista(Lista, ListadiListe, [Lista|ListadiListe]).

%	aggiungi_parola(+Lista, +Parola, ?NuovaLista)
%	NuovaLista è Lista con l'aggiunta di Parola alla fine di essa
aggiungi_parola([], Parola, [Parola]).
aggiungi_parola(Lista, Parola, NuovaLista) :- append(Lista, [Parola], NuovaLista).

%	dividi_frasi(+ListaParole, -FraseParziale, ?ListaFrasi, +Farmaco) (meta-predicato)
%	divide ListaParole in una lista di frasi (rappresentate a loro volta come una lista)
%	ListaParole rappresenta il foglio illustrativo relativo al farmaco Farmaco
%	successivamente lancia `analizza_frasi` con la lista di frasi trovata
dividi_frasi([], [], L, NomeFarmaco) :-
	sort(L, L2),
    analizza_frasi(L2, NomeFarmaco), !.

dividi_frasi([Parola|C], FraseIncompleta, ListaFrasi, NomeFarmaco):-
	(fine_frase(Parola); last([Parola|C], Parola)),
    aggiungi_parola(FraseIncompleta, Parola, FraseCompleta),
    aggiungi_lista(FraseCompleta, ListaFrasi, ListaFrasiNuova),
	dividi_frasi(C, [], ListaFrasiNuova, NomeFarmaco), !.
dividi_frasi([Parola|C], FraseIncompleta, ListaFrasi, NomeFarmaco):-
    aggiungi_parola(FraseIncompleta, Parola, FraseIncompletaNuova),
    dividi_frasi(C, FraseIncompletaNuova, ListaFrasi, NomeFarmaco), !.

%	analizza_frase(+Frase, +Farmaco)
%	Analizza la frase `Frase`, controllando se contiene delle parole chiave.
%	In questo caso fa un `assertz(frase_rilevante(Frase, Farmaco))` 
analizza_frase([], _).
analizza_frase([Parola|C], NomeFarmaco) :-
    parola_chiave(Parola),
	\+ frase_rilevante(NomeFarmaco, [Parola|C]), % effettuo questo controllo perchè 
	% alcuni farmaci possono avere la stessa frase ripetuta, oppure ci possono essere 
	% farmaci con stesso nome, ditta e foglio ilustrativo ma principio attivo diverso
	assertz(frase_rilevante(NomeFarmaco, [Parola|C])).
analizza_frase([_|C], NomeFarmaco) :-
	analizza_frase(C, NomeFarmaco).

%	analizza_frasi(+ListadiFrasi, +Farmaco) (meta-predicato)
%	Esegue `analizza_frase` per ogni frase in `ListadiFrasi`
%	`ListadiFrasi` è l'output relativo a `dividi_frasi/4` per il farmaco `Farmaco`. 
analizza_frasi([], _).
analizza_frasi([Frase|C], NomeFarmaco) :- 
	analizza_frase(Frase, NomeFarmaco),
	analizza_frasi(C, NomeFarmaco).

%	cerca_farmaco(+Malattia, ?NomeFarmaco, ?Frase, -Indice)
%	`Malattia` è una lista di parole che descrivono la malattia o i sintomi.
%	`Frase` è la frase del foglio illustrativo del farmaco `NomeFarmaco` che contiene le parole presenti in `Malattia`	
cerca_farmaco([], _, _, _).
cerca_farmaco([Parola], NomeFarmaco, Frase, Indice):-
	frase_rilevante(NomeFarmaco, Frase),
	once(nth0(Indice, Frase, Parola)), !.
cerca_farmaco([Parola|C], NomeFarmaco, Frase, Indice) :-
	frase_rilevante(NomeFarmaco, Frase),
	nth0(Indice, Frase, Parola),
	Ind is Indice + 1,
	cerca_farmaco(C, NomeFarmaco, Frase, Ind).

%	cerca_farmaco2(+Malattia, ?NomeFarmaco, ?Frase)
%	`Malattia` è una singola parola che descrive la malattia o il sintomo
%	`Frase` è la frase del foglio illustrativo del farmaco `NomeFarmaco` che contiene le parola `Malattia`	
cerca_farmaco2(Malattia, NomeFarmaco, Frase) :-
	frase_rilevante(NomeFarmaco, Frase),
	memberchk(Malattia, Frase).
