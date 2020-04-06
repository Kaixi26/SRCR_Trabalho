
% SICStus flags

:- set_prolog_flag( discontiguous_warnings,off ).
:- set_prolog_flag( single_var_warnings,off ).
:- set_prolog_flag( unknown,fail ).

% PROLOG declarations

:- op( 900,xfy,'::' ).
:- dynamic adjudicante/4.
:- dynamic adjudicataria/4.
:- dynamic contrato/9.

% Extensão do predicado adjudicante: #IdAd, Nome, NIF, Morada -> {V, F, D}
adjudicante(1, 'município de alto de basto', 705330336, 'portugal,braga, alto de basto').

% Extensão do predicado adjudicatária: #IdAda, Nome, NIF, Morada -> {V, F, D}
adjudicataria(1, 'xxx - associados - sociedade de advogados, sp, rl.', 702675112, 'portugal').

% Extensão do predicado contrato:
% #IdAda, TipoDeContrato, TipoDeProcedimento, Descrição, Custo, Prazo, Local, Data
%    -> {V, F, D}
contrato( 705330336, 702675112
        , 'aquisicao de servicos', 'consulta previa'
        , 'assessoria juridica', 13599
        , 547, 'alto de basto', '11-02-2020').

contrato( 705330336, 702675112
        , 'aquisicao de servicos', 'ajuste direto'
        , 'assessoria juridica', 400
        , 300, 'alto de basto', '11-02-2020').


% Invariante : Contrato por ajuste direto tem que ter valor igual ou inferior a 5000€
+contrato(_, _, _, _, _, _, _, _, _) ::
    (findall(Val, contrato(_, _, _, 'ajuste direto', _, Val, _, _, _), L),
    lesseq_than(5000, L)).

% Invariante : Contrato por ajuste direto tem que ser do tipo 'aquisicao de servicos',
% 'aquisicao de bens moveis' ou 'locacao de bens moveis'
+contrato(_, _, _, _, _, _, _, _, _) ::
    (findall(TC, contrato(_, _, TC, 'ajuste direto', _, _, _, _, _), L),
    elemL(L, ['aquisicao de servicos', 'aquisicao de bens moveis', 'locacao de bens moveis'])).

% Invariante : Contrato por ajuste direto tem que ter prazo ate 1 ano
+contrato(_, _, _, _, _, _, _, _, _) ::
    (findall(Pzo, contrato(_, _, _, 'ajuste direto', _, _, Pzo, _, _), L),
    lesseq_than(365, L)).

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
