:- [farmaci].
:- [parole_chiave].
:- dynamic frase_rilevante/2.
:- retractall(frase_rilevante(_,_)).

%estrae tutte le frasi rilevanti dai fogli illustrativi
carica_frasi() :-
	farmaco(NomeFarmaco,_,_,FoglioIllustrativo),
	dividi_frasi(FoglioIllustrativo, [], [], NomeFarmaco).

inizializza() :- findall(_, carica_frasi(), _).

%controlla se la parola è l ultima della frase
fine_frase(P) :-
    sub_atom(P,_,1,0,'.').

%Aggiunge una lista ad una lista di liste
aggiungi_lista(Lista, ListadiListe, [Lista|ListadiListe]).

%Aggiunge una parola ad una lista
aggiungi_parola([], Parola, [Parola]).
aggiungi_parola(Lista, Parola, NuovaLista) :- append(Lista, [Parola], NuovaLista).

%divide una lista di parole in una lista di liste di frasi
%successivamente lancia `analizza_frasi` con la lista trovata
dividi_frasi([], [], L, NomeFarmaco) :- analizza_frasi(L, NomeFarmaco).

dividi_frasi([Parola|C], FraseIncompleta, ListaFrasi, NomeFarmaco):-
    (fine_frase(Parola); last([Parola|C], Parola)),
    aggiungi_parola(FraseIncompleta, Parola, FraseCompleta),
    aggiungi_lista(FraseCompleta, ListaFrasi, ListaFrasiNuova),
	dividi_frasi(C, [], ListaFrasiNuova, NomeFarmaco), !.
dividi_frasi([Parola|C], FraseIncompleta, ListaFrasi, NomeFarmaco):-
    aggiungi_parola(FraseIncompleta, Parola, FraseIncompletaNuova),
    dividi_frasi(C, FraseIncompletaNuova, ListaFrasi, NomeFarmaco), !.

lanciatore(NomeFarmaco):-
	farmaco(NomeFarmaco,_,_,FoglioIllustrativo),
	dividi_frasi(FoglioIllustrativo, [], [], NomeFarmaco).

 
%Analizza una frase per trovare quelle con le parole di interesse, 
%che mi fanno capire a cosa serve il farmaco
analizza_frase([], _).
analizza_frase([Parola|C], NomeFarmaco) :-
    parola_chiave(Parola),
	assertz(frase_rilevante(NomeFarmaco, [Parola|C])), 
	analizza_frase([], _).
analizza_frase([_|C], NomeFarmaco) :-
	analizza_frase(C, NomeFarmaco).

%Richiama `analizza_frase` per ogni lista della lista di liste
analizza_frasi([], _).
analizza_frasi([Frase|C], NomeFarmaco) :- 
	analizza_frase(Frase, NomeFarmaco),
	analizza_frasi(C, NomeFarmaco).
/*
cerca_malattia(Malattia, NomeFarmaco, Frase) :-
	farmaco(NomeFarmaco,_,_,_),
	frase_rilevante(NomeFarmaco, Frase),
	memberchk(Malattia, Frase).
*/
cerca_malattia([], _, _, _).
cerca_malattia([Parola|C], NomeFarmaco, Frase, Indice) :-
	farmaco(NomeFarmaco,_,_,_),
	frase_rilevante(NomeFarmaco, Frase),
	nth0(Indice, Frase, Parola),
	Ind is Indice + 1,
	cerca_malattia(C, NomeFarmaco, Frase, Ind).
