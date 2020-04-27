
% SICStus flags

:- style_check(-singleton).
:- style_check(-discontiguous).


%:- set_prolog_flag( discontiguous_warnings,off ).
%:- set_prolog_flag( single_var_warnings,off ).
%:- set_prolog_flag( unknown,fail ).

% PROLOG declarations

:- op( 900,xfy,'::' ).
:- dynamic adjudicante/4.
:- dynamic adjudicataria/4.
:- dynamic contrato/10.

% Extensão do predicado adjudicante: #IdAd, Nome, NIF, Morada -> {V, F, D}
adjudicante(1, 'município de alto de basto', 705330336, 'portugal,braga, alto de basto').
adjudicante(2, 'município de alto de basto', 705330336, 'portugal,braga, alto de basto').

% Extensão do predicado adjudicatária: #IdAda, Nome, NIF, Morada -> {V, F, D}
adjudicataria(1, 'xxx - associados - sociedade de advogados, sp, rl.', 702675112, 'portugal').
adjudicataria(1, 'xxx - associados - sociedade de advogados, sp, rl.', 702675112, 'portugal').

% Extensão do predicado contrato:
% #IdContrato, #IdAd, #IdAda, TipoDeContrato, TipoDeProcedimento, Descrição, Custo, Prazo, Local, Data
%    -> {V, F, D}
contrato( 1, 705330336, 702675112
        , 'aquisicao de servicos', 'consulta previa'
        , 'assessoria juridica', 13599
        , 547, 'alto de basto', 11-02-2020).

contrato( 1, 705330336, 702675112
        , 'aquisicao de servicos', 'ajuste direto'
        , 'assessoria juridica', 400
        , 300, 'alto de basto', 11-02-2020).

% Invariante : Verifica que não existe mais que um id sem clausulas desconhecidas
+adjudicante(_, _, _, _) ::
    (findall(Id, demo(adjudicante(Id, _, _, _), verdadeiro), L),
    remRep(L, Lnonrep),
    eqList(L, Lnonrep)).
+adjudicataria(_, _, _, _) ::
    (findall(Id, demo(adjudicataria(Id, _, _, _), verdadeiro), L),
    remRep(L, Lnonrep),
    eqList(L, Lnonrep)).
contrato(Id, _, _, _, _, _, _, _, _, _) ::
    (findall(Id, demo(contrato(Id, _, _, _, _, _, _, _, _, _), verdadeiro), L),
    remRep(L, Lnonrep),
    eqList(L, Lnonrep)).


% Invariante : Verifica que os tipos de procedimento são válidos
% 'ajuste direto' ,'consulta previa' ,'concurso publico'
+contrato(_, _, _, _, _, _, _, _, _, _) ::
    (findall(TP, contrato(_, _, _, _, TP, _, _, _, _, _), L),
    elemL(L, ['ajuste direto' ,'consulta previa' ,'concurso publico'])).

% Invariante : Contrato por ajuste direto tem que ter valor igual ou inferior a 5000€
+contrato(_, _, _, _, _, _, _, _, _, _) ::
    (findall(Val, contrato(_, _, _, _, 'ajuste direto', _, Val, _, _, _), L),
    lesseq_than(5000, L)).

% Invariante : Contrato por ajuste direto tem que ser do tipo 'aquisicao de servicos',
% 'aquisicao de bens moveis' ou 'locacao de bens moveis'
+contrato(_, _, _, _, _, _, _, _, _, _) ::
    (findall(TC, contrato(_, _, _, TC, 'ajuste direto', _, _, _, _, _), L),
    elemL(L, ['aquisicao de servicos', 'aquisicao de bens moveis', 'locacao de bens moveis'])).

% Invariante : Contrato por ajuste direto tem que ter prazo ate 1 ano
+contrato(_, _, _, _, _, _, _, _, _, _) ::
    (findall(Pzo, contrato(_, _, _, _, 'ajuste direto', _, _, Pzo, _, _), L),
    lesseq_than(365, L)).

% Invariante : Regra dos 3 anos
+contrato(_, Ad, Ada, _, _, Desc, _, _, _, Date) ::
    (findall(temp(Custo, Date, OldDate), contrato(_, Ad, Ada, _, _, Desc, Custo, _, _, OldDate), S), aux(S, R), add(R, V), V < 75000).

aux([], []).
aux([temp(Custo, Date, OldDate)|T], aux(T)) :- diff_years(Date, OldDate, D), D > 2.
aux([temp(Custo, _, _)|T], [Custo|aux(T)]). 

add([], SUM) :- SUM is 0.
add([H|T], SUM) :- add(T, X), SUM is H + X.

% findall([Ad, Tp, Val, Data], contrato(_, Ad, _, _, Tp, _, _, _, _, Data), L).

% insert_3yrule([Ad, Tp, Val], [], [Ad, [Tp, Val]]).
% insert_3yrule([Ad, Tp, Val], [], [Ad, [Tp, Val]]).

% filter_date_2y([[a,1,2,1-2-2018], [b,2,3,1-1-2018], [c,4,5,1-1-2019], [d,5,6,1-1-2020]], 15-1-2020, L).
filter_date_2y([], Date, []).
filter_date_2y([[Ad, Tp, Val, Date1]|Xs], Date2, [[Ad, Tp, Val]|FXs]) :-
    diff_date(Date2, Date1, Days),
    Days > 0,
    Days =< 730,
    filter_date_2y(Xs, Date2, FXs).
filter_date_2y([[Ad, Tp, Val, Date1]|Xs], Date2, FXs) :-
    diff_date(Date2, Date1, Days),
    Days > 0,
    filter_date_2y(Xs, Date2, FXs).

diff_years(D1-M1-Y1, D2-M2-Y2, Years) :-
    Years is (Y1 - Y2).

% Testa se todos os elementos de uma lista são menores do que um elemento
lesseq_than(N, []).
lesseq_than(N, [H|T]) :-
    H =< N,
    lesseq_than(N, T).

% Testa se um elemento está na lista
elem(E, []) :- fail.
elem(E, [E|_]).
elem(E, [_|T]) :- elem(E, T).

% Testa se todos os elementos da primeira lista pertencem à segunda lista
elemL([], L).
elemL([LE|LES], L) :-
    elem(LE, L),
    elemL(LES, L).

% Remove os elementos repetidos de uma lista
 remRep([], []).
 remRep([L|Ls], [L|Lret]) :-
     remElem(L, Ls, Tmp),
     remRep(Tmp, Lret).

% Remove todas as ocorrencias de um elemento numa lista
remElem(E, [], []).
remElem(E, [E|Ls], Lr) :- remElem(E, Ls, Lr).
remElem(E, [L|Ls], [L|Lr]) :- E \= L, remElem(E, Ls, Lr).

% Testa se duas listas são iguais
eqList([], []).
eqList([L1|Ls1], [L1|Ls2]) :- eqList(Ls1, Ls2).
eqList([L1|_], [L2|_]) :- fail.

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do meta-predicado demo: Questao,Resposta -> {V,F}
%                            Resposta = { verdadeiro,falso,desconhecido }
demo( Questao,verdadeiro ) :-
    Questao.
demo( Questao,falso ) :-
    -Questao.
demo( Questao,desconhecido ) :-
    nao( Questao ),
    nao( -Questao ).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do meta-predicado nao: Questao -> {V,F}

nao( Questao ) :-
    Questao, !, fail.
nao( Questao ).