
% SICStus flags

:- style_check(-singleton).
:- style_check(-discontiguous).


%:- set_prolog_flag( discontiguous_warnings,off ).
%:- set_prolog_flag( single_var_warnings,off ).
%:- set_prolog_flag( unknown,fail ).

% PROLOG declarations

:- dynamic '-'/1.
:- op( 900,xfy,'::' ).
:- dynamic adjudicante/4.
:- dynamic adjudicataria/4.
:- dynamic contrato/10.

% --------------------------------------------------------------------------------
% Definicao de predicados
% --------------------------------------------------------------------------------

% Extensão do predicado adjudicante: #IdAd, Nome, NIF, Morada -> {V, F, D}
adjudicante(1,  'município de alto de basto',           705330336, 'portugal,braga, alto de basto').
adjudicante(2,  'município de ponte de lima',           705330331, 'portugal,viana do castelo, ponte de lima').
adjudicante(3,  'município de ponte da barca',          705330332, 'portugal,viana do castelo, ponte da barca').
adjudicante(4,  'município de arcos de valdevez',       705330333, 'portugal,viana do castelo, arcos de valdevez').
adjudicante(5,  'município de viana do castelo',        705330334, 'portugal,viana do castelo, viana do castelo').
adjudicante(6,  'município de caminha',                 705330335, 'portugal,viana do castelo, caminha').
adjudicante(7,  'município de moncao',                  705330337, 'portugal,viana do castelo, moncao').         
adjudicante(8,  'município de melgaco',                 705330338, 'portugal,viana do castelo, melgaco').         
adjudicante(9,  'município de valenca',                 705330339, 'portugal,viana do castelo, valenca').         
adjudicante(10, 'município de vila praia de ancora',    705330340, 'portugal,viana do castelo, vila praia de ancora').
adjudicante(11, 'município de paredes de coura',        705330341, 'portugal,viana do castelo, paredes de coura').
adjudicante(12, 'município de vila nova de cerveira',   705330342, 'portugal,viana do castelo, vila nova de cerveira').
adjudicante(13, 'município de amares',                  705330343, 'portugal,braga, amares').
adjudicante(14, 'município de braga',                   705330344, 'portugal,braga, braga').
adjudicante(15, 'município de barcelos',                705330345, 'portugal,braga, barcelos').
adjudicante(16, 'município de fafe',                    705330346, 'portugal,braga, fafe').
adjudicante(17, 'município de guimaraes',               705330347, 'portugal,braga, guimaraes').
adjudicante(18, 'município de esposende',               705330348, 'portugal,braga, esposende').
adjudicante(19, 'município de vieira do minho',         705330349, 'portugal,braga, vieira do minho').
adjudicante(20, 'município de vizela',                  705330350, 'portugal,braga, vizela').
adjudicante(21, 'município de vila verde',              705330351, 'portugal,braga, vila verde').




% Extensão do predicado adjudicatária: #IdAda, Nome, NIF, Morada -> {V, F, D}
adjudicataria(1,  'xxx - associados - sociedade de advogados, sp, rl.',     702675112, 'portugal').
adjudicataria(2,  'xxx - associados - sociedade de professores, sp, rl.',   702675111, 'portugal').
adjudicataria(3,  'xxx - associados - sociedade de engenheiros, sp, rl.',   702675113, 'portugal').
adjudicataria(4,  'xxx - associados - sociedade de enfermeiros, sp, rl.',   702675114, 'portugal').
adjudicataria(5,  'xxx - associados - sociedade de medicos, sp, rl.',       702675115, 'portugal').
adjudicataria(6,  'xxx - associados - sociedade de dentistas, sp, rl.',     702675116, 'portugal').
adjudicataria(7,  'xxx - associados - sociedade de agricultores, sp, rl.',  702675117, 'portugal').
adjudicataria(8,  'xxx - associados - sociedade de mecanicos, sp, rl.',     702675118, 'portugal').
adjudicataria(9,  'xxx - associados - sociedade de pescadores, sp, rl.',    702675119, 'portugal').
adjudicataria(10, 'xxx - associados - sociedade de trolhas, sp, rl.',       702675120, 'portugal').
adjudicataria(11, 'xxx - associados - sociedade de carpinteiros, sp, rl.',  702675121, 'portugal').
adjudicataria(12, 'xxx - associados - sociedade de bombeiros, sp, rl.',     702675122, 'portugal').
adjudicataria(13, 'xxx - associados - sociedade de cozinheiros, sp, rl.',   702675123, 'portugal').
adjudicataria(14, 'xxx - associados - sociedade de costureiras, sp, rl.',   702675124, 'portugal').
adjudicataria(15, 'xxx - associados - sociedade de pintores, sp, rl.',      702675125, 'portugal').
adjudicataria(16, 'xxx - associados - sociedade de arquitetos, sp, rl.',    702675126, 'portugal').
adjudicataria(17, 'xxx - associados - sociedade de padeiros, sp, rl.',      702675127, 'portugal').
adjudicataria(18, 'xxx - associados - sociedade de treinadores, sp, rl.',   702675128, 'portugal').
adjudicataria(19, 'xxx - associados - sociedade de jogadores, sp, rl.',     702675129, 'portugal').
adjudicataria(20, 'xxx - associados - sociedade de policias, sp, rl.',      702675130, 'portugal').
adjudicataria(21, 'xxx - associados - sociedade de auxiliares, sp, rl.',    702675131, 'portugal').


% Extensão do predicado contrato:
% #IdContrato, #IdAd, #IdAda, TipoDeContrato, TipoDeProcedimento, Descricão, Custo, Prazo, Local, Data
%    -> {V, F, D}
contrato(1,  1,  21, 'aquisicao de servicos'    , 'consulta previa'  , 'assessoria juridica',      13599  , 547, 'alto de basto',         11-01-2020).
contrato(2,  2,  20, 'aquisicao de servicos'    , 'ajuste direto'    , 'assessoria juridica',      400    , 300, 'ponte de lima',         12-02-2020).
contrato(3,  3,  19, 'aquisicao de servicos'    , 'concurso publico' , 'assessoria juridica',      1600   , 200, 'ponte da barca',        13-03-2020).
contrato(4,  4,  18, 'aquisicao de servicos'    , 'consulta previa'  , 'assessoria juridica',      300    , 360, 'arcos de valdevez',     14-04-2020).
contrato(5,  5,  17, 'aquisicao de servicos'    , 'ajuste direto'    , 'assessoria juridica',      200    , 100, 'viana de castelo',      15-05-2019).
contrato(6,  6,  16, 'aquisicao de bens moveis' , 'concurso publico' , 'assessoria juridica',      40530  , 500, 'caminha',               16-06-2018).
contrato(7,  7,  15, 'aquisicao de bens moveis' , 'consulta previa'  , 'assessoria juridica',      1999   , 547, 'moncao',                17-07-2018).
contrato(8,  8,  14, 'aquisicao de bens moveis' , 'ajuste direto'    , 'assessoria juridica',      30     , 280, 'melgaco',               18-08-2019).
contrato(9,  9,  13, 'aquisicao de bens moveis' , 'concurso publico' , 'assessoria juridica',      799    , 657, 'valenca',               19-09-2017).
contrato(10, 10, 12, 'aquisicao de bens moveis' , 'consulta previa'  , 'assessoria juridica',      8900   , 90,  'vila praia de ancora',  20-10-2018).
contrato(11, 11, 11, 'aquisicao de bens moveis' , 'ajuste direto'    , 'aquisicao de equipamentos', 1500   , 79,  'paredes de coura',      21-11-2019).
contrato(12, 12, 10, 'aquisicao de bens moveis' , 'concurso publico' , 'aquisicao de equipamentos', 6701   , 890, 'vila nova de cerveira', 22-12-2017).
contrato(13, 13, 9,  'aquisicao de bens moveis' , 'consulta previa'  , 'aquisicao de equipamentos', 30000  , 345, 'amares',                23-01-2018).
contrato(14, 14, 8,  'aquisicao de bens moveis' , 'ajuste direto'    , 'aquisicao de equipamentos', 4300   , 300, 'braga',                 24-02-2019).
contrato(15, 15, 7,  'locacao de bens moveis'   , 'concurso publico' , 'aquisicao de equipamentos', 25700  , 265, 'barcelos',              25-03-2017).
contrato(16, 16, 6,  'locacao de bens moveis'   , 'consulta previa'  , 'aquisicao de equipamentos', 19450  , 490, 'fafe',                  26-04-2018).
contrato(17, 17, 5,  'locacao de bens moveis'   , 'ajuste direto'    , 'aquisicao de equipamentos', 3700   , 310, 'guimaraes',             27-05-2019).
contrato(18, 18, 4,  'locacao de bens moveis'   , 'concurso publico' , 'aquisicao de equipamentos', 100000 , 123, 'esposende',             28-06-2017).
contrato(19, 19, 3,  'locacao de bens moveis'   , 'consulta previa'  , 'aquisicao de equipamentos', 9030   , 456, 'vieira do minho',       29-07-2018).
contrato(20, 20, 2,  'locacao de bens moveis'   , 'ajuste direto'    , 'aquisicao de equipamentos', 790    , 190, 'vizela',                30-08-2019).
contrato(21, 21, 1,  'locacao de bens moveis'   , 'concurso publico' , 'aquisicao de equipamentos', 12345  , 789, 'vila verde',            30-09-2019).

% --------------------------------------------------------------------------------
% Definicao de invariantes
% --------------------------------------------------------------------------------

% Invariante : Verifica que não existe mais que um id sem clausulas desconhecidas
+adjudicante(_, _, _, _) ::
    (findall(Id, demo(adjudicante(Id, _, _, _), verdadeiro), L),
    remRep(L, Lnonrep),
    eqList(L, Lnonrep)).
+adjudicataria(_, _, _, _) ::
    (findall(Id, demo(adjudicataria(Id, _, _, _), verdadeiro), L),
    remRep(L, Lnonrep),
    eqList(L, Lnonrep)).
+contrato(Id, _, _, _, _, _, _, _, _, _) ::
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
    (findall(Pzo, demo(contrato(_, _, _, _, 'ajuste direto', _, _, Pzo, _, _), verdadeiro), L),
    lesseq_than(365, L)).

% Invariante : Regra dos 3 anos
+contrato(_, Ad, Ada, _, _, Desc, _, _, _, Date) ::
    (findall(temp(Custo, Date, OldDate), contrato(_, Ad, Ada, _, _, Desc, Custo, _, _, OldDate), S), aux(S, R), add(R, V), V < 75000).


% --------------------------------------------------------------------------------
% Auxiliares para os invariantes
% --------------------------------------------------------------------------------

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
% lesseq_than: Valor, Lista -> {V, F}
lesseq_than(N, []).
lesseq_than(N, [H|T]) :-
    H =< N,
    lesseq_than(N, T).

% Testa se um elemento está na lista
% elem: Elemento, Lista -> {V, F}
elem(E, []) :- fail.
elem(E, [E|_]).
elem(E, [_|T]) :- elem(E, T).

% Testa se todos os elementos da primeira lista pertencem à segunda lista
% elemL: Lista, Lista -> {V, F}
elemL([], L).
elemL([LE|LES], L) :-
    elem(LE, L),
    elemL(LES, L).

% Remove os elementos repetidos de uma lista
% remRep: Lista, Resultado -> {V,F}
 remRep([], []).
 remRep([L|Ls], [L|Lret]) :-
     remElem(L, Ls, Tmp),
     remRep(Tmp, Lret).

% Remove todas as ocorrencias de um elemento numa lista
% remElem: Elemento, Lista, Resultado -> {V, F}
remElem(E, [], []).
remElem(E, [E|Ls], Lr) :- remElem(E, Ls, Lr).
remElem(E, [L|Ls], [L|Lr]) :- E \= L, remElem(E, Ls, Lr).

% Testa se duas listas são iguais
% eqList: Lista, Lista -> {V, F}
eqList([], []).
eqList([L1|Ls1], [L1|Ls2]) :- eqList(Ls1, Ls2).
eqList([L1|_], [L2|_]) :- fail.

% --------------------------------------------------------------------------------
% Queries extra para a base de conhecimento
% --------------------------------------------------------------------------------

% predicado que devolve o número total de adjudicantes
% total_adjudicantes: Resultado -> {V,F}
total_adjudicantes(Tot) :- findall(Id, adjudicante(Id, _, _, _), L),
    length(L, Tot).

% predicado que devolve o número total de adjudicatarias
% total_adjudicatarias: Resultado -> {V,F}
total_adjudicatarias(Tot) :- findall(Id, adjudicataria(Id, _, _, _), L),
    length(L, Tot).

% predicado que devolve o número total de contratos
% total_contratos: Resultado -> {V,F}
total_contratos(Tot) :- findall(Id, contrato(Id, _, _, _, _, _, _, _, _, _), L),
    length(L, Tot).

% predicado que devolve o número total de contratos apos uma data
% total_contratos_apos: Data, Resultado -> {V,F}
total_contratos_apos(Date, Res) :- 
    contratos_apos(Date, L),
    length(L, Res).

% predicado que devolve o número total de contratos antes de uma data
% total_contratos_antes: Data, Resultado -> {V,F}
total_contratos_antes(Date, Res) :- 
    contratos_antes(Date, L),
    length(L, Res).

% predicado que devolve o número total de contratos de um adjudicante
% total_contratos_adjudicante: IdAdj, Resultado -> {V,F}
total_contratos_adjudicante(IdAdj, Res) :- 
    contratos_adjudicante(IdAdj, L),
    length(L, Res).

% predicado que devolve o número total de contratos de uma adjudicataria
% total_contratos_adjudicataria: IdAdj, Resultado -> {V,F}
total_contratos_adjudicataria(IdAdj, Res) :- 
    contratos_adjudicataria(IdAdj, L),
    length(L, Res).

% predicado que devolve o número total de contratos de um tipo
% total_contratos_tipo: Tipo, Resultado -> {V,F}
total_contratos_tipo(Tipo, Res) :- 
    contratos_tipo(Tipo, L),
    length(L, Res).


% predicado que devolve o número total de contratos de um tipo de procedimento
% total_contratos_tipo_procedimento: Tipo, Resultado -> {V,F}
total_contratos_tipo_procedimento(Tipo, Res) :- 
    contratos_tipo_procedimento(Tipo, L),
    length(L, Res).

% predicado que devolve o número total de contratos com valor acima de
% total_contratos_acima: Valor, Resultado -> {V,F}
total_contratos_valor_acima(Valor, Res) :- 
    contratos_valor_acima(Valor, L),
    length(L, Res).

% predicado que devolve o número total de contratos com valor abaixo de
% total_contratos_abaixo: Valor, Resultado -> {V,F}
total_contratos_valor_abaixo(Valor, Res) :- 
    contratos_valor_abaixo(Valor, L),
    length(L, Res).

% predicado que devolve o número total de contratos com prazo acima de
% total_contratos_prazo_acima: Prazo, Resultado -> {V,F}
total_contratos_prazo_acima(Valor, Res) :- 
    contratos_prazo_acima(Valor, L),
    length(L, Res).

% predicado que devolve o número total de contratos com prazo abaixo de
% total_contratos_prazo_abaixo: Prazo, Resultado -> {V,F}
total_contratos_prazo_abaixo(Valor, Res) :- 
    contratos_prazo_abaixo(Valor, L),
    length(L, Res).

% predicado que devolve contratos apos uma data
% contratos_apos_data: Data, Resultado -> {V,F}
contratos_apos(Date, Res) :-
    findall(contrato(A, B, C, D, E, F, G, H, I, CDate), demo(contrato(A, B, C, D, E, F, G, H, I, CDate), verdadeiro), L),
    filter_date_is_after(Date, L, Res).

% predicado que devolve contratos antes de uma data
% contratos_apos_data: Data, Resultado -> {V,F}
contratos_antes(Date, Res) :-
    findall(contrato(A, B, C, D, E, F, G, H, I, CDate), demo(contrato(A, B, C, D, E, F, G, H, I, CDate), verdadeiro), L),
    filter_date_is_before(Date, L, Res).

% predicado que devolve contratos de um adjudicante
% contratos_adjudicante: IdAd, Resultado -> {V,F}
contratos_adjudicante(IdAd, Res) :-
    findall(contrato(A, B, C, D, E, F, G, H, I, J), demo(contrato(A, B, C, D, E, F, G, H, I, J), verdadeiro), L),
    filter_contrato_adjudicante(IdAd, L, Res).

% predicado que devolve contratos de uma adjudicataria
% contratos_adjudicante: IdAd, Resultado -> {V,F}
contratos_adjudicataria(IdAd, Res) :-
    findall(contrato(A, B, C, D, E, F, G, H, I, J), demo(contrato(A, B, C, D, E, F, G, H, I, J), verdadeiro), L),
    filter_contrato_adjudicataria(IdAd, L, Res).

% predicado que devolve contratos de um tipo
% contratos_tipo: Tipo, Resultado -> {V,F}
contratos_tipo(Tipo, Res) :-
    findall(contrato(A, B, C, D, E, F, G, H, I, J), demo(contrato(A, B, C, D, E, F, G, H, I, J), verdadeiro), L),
    filter_contrato_tipo(Tipo, L, Res).

% predicado que devolve contratos de um tipo de procedimento
% contratos_tipo_procedimento: Tipo, Resultado -> {V,F}
contratos_tipo_procedimento(Tipo, Res) :-
    findall(contrato(A, B, C, D, E, F, G, H, I, J), demo(contrato(A, B, C, D, E, F, G, H, I, J), verdadeiro), L),
    filter_contrato_tipo_procedimento(Tipo, L, Res).

% predicado que devolve contratos acima de um valor
% contratos_valor_acima: Tipo, Resultado -> {V,F}
contratos_valor_acima(Valor, Res) :-
    findall(contrato(A, B, C, D, E, F, G, H, I, J), demo(contrato(A, B, C, D, E, F, G, H, I, J), verdadeiro), L),
    filter_contrato_valor_acima(Valor, L, Res).

% predicado que devolve contratos abaixo de um valor
% contratos_valor_abaixo: Valor, Resultado -> {V,F}
contratos_valor_abaixo(Valor, Res) :-
    findall(contrato(A, B, C, D, E, F, G, H, I, J), demo(contrato(A, B, C, D, E, F, G, H, I, J), verdadeiro), L),
    filter_contrato_valor_abaixo(Valor, L, Res).

% predicado que devolve contratos acima de um prazo
% contratos_prazo_acima: Prazo, Resultado -> {V,F}
contratos_prazo_acima(Prazo, Res) :-
    findall(contrato(A, B, C, D, E, F, G, H, I, J), demo(contrato(A, B, C, D, E, F, G, H, I, J), verdadeiro), L),
    filter_contrato_prazo_acima(Prazo, L, Res).

% predicado que devolve contratos abaixo de um prazo
% contratos_prazo_abaixo: Prazo, Resultado -> {V,F}
contratos_prazo_abaixo(Prazo, Res) :-
    findall(contrato(A, B, C, D, E, F, G, H, I, J), demo(contrato(A, B, C, D, E, F, G, H, I, J), verdadeiro), L),
    filter_contrato_prazo_abaixo(Prazo, L, Res).

% --------------------------------------------------------------------------------
% Auxiliares para as queries extra da base de conhecimento
% --------------------------------------------------------------------------------

% filtra de uma lista de contratos com apenas contratos após a data
% filter_date_is_after: Data, Contratos, Resultado -> {V, F}
filter_date_is_after(Date, [], []).
filter_date_is_after(Date, [contrato(A, B, C, D, E, F, G, H, I, CDate)|Cs], [contrato(A, B, C, D, E, F, G, H, I, CDate)|FCs]) :-
    date_is_after(CDate, Date),
    filter_date_is_after(Date, Cs, FCs).
filter_date_is_after(Date, [_|Cs], FCs) :-
    filter_date_is_after(Date, Cs, FCs).

% filtra de uma lista de contratos com apenas contratos antes ou na data
% filter_date_is_before: Data, Contratos, Resultado -> {V, F}
filter_date_is_before(Date, [], []).
filter_date_is_before(Date, [contrato(A, B, C, D, E, F, G, H, I, CDate)|Cs], [contrato(A, B, C, D, E, F, G, H, I, CDate)|FCs]) :-
    \+ date_is_after(CDate, Date),
    filter_date_is_before(Date, Cs, FCs).
filter_date_is_before(Date, [_|Cs], FCs) :-
    filter_date_is_before(Date, Cs, FCs).

% filtra de uma lista de contratos com apenas contratos de um adjudicante
% filter_contrato_adjudicante: Adjudicantes, Contratos, Resultado -> {V, F}
filter_contrato_adjudicante(IdAdj, [], []).
filter_contrato_adjudicante(IdAdj, [contrato(A, IdAdj, C, D, E, F, G, H, I, J)|Cs], [contrato(A, IdAdj, C, D, E, F, G, H, I, J)|FCs]) :-
    filter_contrato_adjudicante(IdAdj, Cs, FCs).
filter_contrato_adjudicante(IdAdj, [_|Cs], FCs) :-
    filter_contrato_adjudicante(IdAdj, Cs, FCs).


% filtra de uma lista de contratos com apenas contratos de uma adjudicatária
% filter_contrato_adjudicataria: Adjudicantes, Contratos, Resultado -> {V, F}
filter_contrato_adjudicataria(IdAdj, [], []).
filter_contrato_adjudicataria(IdAdj, [contrato(A, B, IdAdj, D, E, F, G, H, I, J)|Cs], [contrato(A, B, IdAdj, D, E, F, G, H, I, J)|FCs]) :-
    filter_contrato_adjudicataria(IdAdj, Cs, FCs).
filter_contrato_adjudicataria(IdAdj, [_|Cs], FCs) :-
    filter_contrato_adjudicataria(IdAdj, Cs, FCs).

% filtra de uma lista de contratos com apenas contratos de um tipo
% filter_contrato_tipo: Tipo, Contratos, Resultado -> {V, F}
filter_contrato_tipo(Tipo, [], []).
filter_contrato_tipo(Tipo, [contrato(A, B, C, Tipo, E, F, G, H, I, J)|Cs], [contrato(A, B, C, Tipo, E, F, G, H, I, J)|FCs]) :-
    filter_contrato_tipo(Tipo, Cs, FCs).
filter_contrato_tipo(Tipo, [_|Cs], FCs) :-
    filter_contrato_tipo(Tipo, Cs, FCs).

% filtra de uma lista de contratos com apenas contratos de um tipo de procedimento
% filter_contrato_tipo_procedimento: Tipo, Contratos, Resultado -> {V, F}
filter_contrato_tipo_procedimento(Tipo, [], []).
filter_contrato_tipo_procedimento(Tipo, [contrato(A, B, C, D, Tipo, F, G, H, I, J)|Cs], [contrato(A, B, C, D, Tipo, F, G, H, I, J)|FCs]) :-
    filter_contrato_tipo_procedimento(Tipo, Cs, FCs).
filter_contrato_tipo_procedimento(Tipo, [_|Cs], FCs) :-
    filter_contrato_tipo_procedimento(Tipo, Cs, FCs).

% filtra de uma lista de contratos com apenas contratos acima de um valor
% filter_contrato_valor_acima: Valor, Contratos, Resultado -> {V, F}
filter_contrato_valor_acima(Valor, [], []).
filter_contrato_valor_acima(Valor, [contrato(A, B, C, D, E, F, Val, H, I, J)|Cs], [contrato(A, B, C, D, E, F, Val, H, I, J)|FCs]) :-
    Val > Valor,
    filter_contrato_valor_acima(Valor, Cs, FCs).
filter_contrato_valor_acima(Tipo, [_|Cs], FCs) :-
    filter_contrato_valor_acima(Tipo, Cs, FCs).

% filtra de uma lista de contratos com apenas contratos abaixo de um valor
% filter_contrato_valor_abaixo: Valor, Contratos, Resultado -> {V, F}
filter_contrato_valor_abaixo(Valor, [], []).
filter_contrato_valor_abaixo(Valor, [contrato(A, B, C, D, E, F, Val, H, I, J)|Cs], [contrato(A, B, C, D, E, F, Val, H, I, J)|FCs]) :-
    Valor > Val,
    filter_contrato_valor_abaixo(Valor, Cs, FCs).
filter_contrato_valor_abaixo(Tipo, [_|Cs], FCs) :-
    filter_contrato_valor_abaixo(Tipo, Cs, FCs).

% filtra de uma lista de contratos com apenas contratos com prazo acima de um valor
% filter_contrato_prazo_acima: Prazo, Contratos, Resultado -> {V, F}
filter_contrato_prazo_acima(Prazo, [], []).
filter_contrato_prazo_acima(Prazo, [contrato(A, B, C, D, E, F, G, Pzo, I, J)|Cs], [contrato(A, B, C, D, E, F, G, Pzo, I, J)|FCs]) :-
    Prazo < Pzo,
    filter_contrato_prazo_acima(Prazo, Cs, FCs).
filter_contrato_prazo_acima(Prazo, [_|Cs], FCs) :-
    filter_contrato_prazo_acima(Prazo, Cs, FCs).

% filtra de uma lista de contratos com apenas contratos com prazo abaixo de um valor
% filter_contrato_prazo_abaixo: Prazo, Contratos, Resultado -> {V, F}
filter_contrato_prazo_abaixo(Prazo, [], []).
filter_contrato_prazo_abaixo(Prazo, [contrato(A, B, C, D, E, F, G, Pzo, I, J)|Cs], [contrato(A, B, C, D, E, F, G, Pzo, I, J)|FCs]) :-
    Pzo < Prazo,
    filter_contrato_prazo_abaixo(Prazo, Cs, FCs).
filter_contrato_prazo_abaixo(Prazo, [_|Cs], FCs) :-
    filter_contrato_prazo_abaixo(Prazo, Cs, FCs).

% predicado que calcula se a primeira data vem após a segunda
% contratos_apos_data: Data, Data -> {V,F}
date_is_after(D1-M1-Y1, D2-M2-Y2) :- Y1 > Y2.
date_is_after(D1-M1-Y1, D2-M2-Y2) :- M1 > M2.
date_is_after(D1-M1-Y1, D2-M2-Y2) :- D1 > D2.

% --------------------------------------------------------------------------------
% Extensoes para conhecimento incerto
% --------------------------------------------------------------------------------

% Extensao do meta-predicado demo: Questao,Resposta -> {V,F}
%                            Resposta = { verdadeiro,falso,desconhecido }
demo( Questao,verdadeiro ) :-
    Questao.
demo( Questao,falso ) :-
    -Questao.
demo( Questao,desconhecido ) :-
    nao( Questao ),
    nao( -Questao ).

% Extensao do meta-predicado nao: Questao -> {V,F}

nao( Questao ) :-
    Questao, !, fail.
nao( Questao ).

% --------------------------------------------------------------------------------
% Manipulação e introdução de dados
% --------------------------------------------------------------------------------

% Extensão do predicado remove
remove(T) :- retract(T).

% Extensão do predicado insere
insere(T) :- assert(T).
insere(T) :- retract(T),!,fail.

% Extensão do predicado teste
teste([]).
teste([H|T]) :- H, teste(T).

% Extensao do predicado solucoes: F, Q, S -> {V, F}	

solucoes(F, Q, S) :- findall(F, Q, S).

% Extensão do predicado evolução: Termo -> {V,F}
evolucao(T):- solucoes(Inv,+T::Inv,Lista),
			  insere(T),
			  teste(Lista).

% Extensão do predicado involucao: Termo -> {V,F}
involucao(T):- solucoes(I,-T::I,Lista),
				teste(Lista),
				remove(T).

% Extensão para adicionar um adjudicante
% Extensão do predicado adjudicante: #IdAd, Nome, NIF, Morada -> {V, F}
regista_adjudicante(A, B, C, D) :-
    evolucao(adjudicante(A, B, C, D)).

% Extensão para remover um adjudicante e os contratos com o seu Id
% remove_adjudicante: Id -> {V, F}
remove_adjudicante(Id) :-
    involucao(adjudicante(Id,_,_,_)).

% Extensão para adicionar uma adjudicataria
% Extensão do predicado adjudicatária: #IdAda, Nome, NIF, Morada -> {V, F}
regista_adjudicataria(A, B, C, D) :-
    evolucao(adjudicataria(A, B, C, D)).

% Extensão para remover uma adjudicataria e os contratos com o seu Id
% remove_adjudicataria: Id -> {V, F}
remove_adjudicataria(Id) :-
    involucao(adjudicataria(Id,_,_,_)).

% Extensão para registar um contrato
% regista_contrato: #IdContrato, #IdAd, #IdAda, TipoDeContrato, TipoDeProcedimento, Descricão, Custo, Prazo, Local, Data -> {V, F}
regista_contrato(A, B, C, D, E, F, G, H, I, J) :-
    evolucao(contrato(A, B, C, D, E, F, G, H, I, J)).

% Extensão para remover um contrato através do seu Id
% remove_contrato: Id -> {V, F}
remove_contrato(Id) :-
    involucao(contrato(Id, _, _, _, _, _, _, _, _, _)).


% --------------------------------------------------------------------------------
% Extensões de conhecimento incerto relativos aos dados
% --------------------------------------------------------------------------------

-adjudicante(A, B, C, D) :-
    \+ adjudicante(A, B, C, D),
    \+ excecao(adjudicante(A, B, C, D)).

-adjudicataria(A, B, C, D) :-
    \+ adjudicante(A, B, C, D),
    \+ excecao(adjudicante(A, B, C, D)).

-contrato(A, B, C, D, E, F, G, H, I, J) :-
    \+ contrato(A, B, C, D, E, F, G, H, I, J),
    \+ excecao(contrato(A, B, C, D, E, F, G, H, I, J)).

% --------------------------------------------------------------------------------
% Conhecimento Incerto
% --------------------------------------------------------------------------------

adjudicante(22, 'município de alto de basto', 705331234, morada_desconhecida).
adjudicante(23, 'município de ponte de lima', 705334567, morada_desconhecida).

excecao(adjudicante(A, B, C, D)) :-
    adjudicante(A, B, C, morada_desconhecida).


adjudicataria(22,  'xxx - associados - sociedade de advogados, sp, rl.',     702671234, 'portugal').
adjudicataria(23,  'xxx - associados - sociedade de professores, sp, rl.',   702679876, 'portugal').

excecao(adjudicataria(A, B, C, D)) :-
    adjudicataria(A, B, C, morada_desconhecida).


contrato(22, 1, 21, 'aquisicao de servicos', 'consulta previa', 'assessoria juridica', 13599, 547, local_desconhecido, 11-01-2020).
contrato(23, 1, 21, 'aquisicao de servicos', 'consulta previa', 'assessoria juridica', 13599, 547, local_desconhecido, 11-01-2020).

excecao(contrato(A, B, C, D, E, F, G, H, I, J)) :-
    contrato(A, B, C, D, E, F, G, H, local_desconhecido, J).

% --------------------------------------------------------------------------------
% Conhecimento Impreciso
% --------------------------------------------------------------------------------

excecao(adjudicante(24, 'município de alto de basto', 705331234, 'portugal,braga, alto de basto')).
excecao(adjudicante(24, 'município de alto de basto', 705331234, 'portugal,viana do castelo, ponte de lima')).


excecao(adjudicataria(24,  'xxx - associados - sociedade de advogados, sp, rl.', 702675112, 'portugal,braga, alto de basto')).
excecao(adjudicataria(24,  'xxx - associados - sociedade de advogados, sp, rl.', 702675112, 'portugal,viana do castelo, ponte de lima')).


excecao(contrato(24, 1, 21, 'aquisicao de servicos', 'consulta previa', 'assessoria juridica', 13599, 547, 'portugal,braga, alto de basto' , 11-01-2020)).
excecao(contrato(24, 1, 21, 'aquisicao de servicos', 'consulta previa', 'assessoria juridica', 13599, 547, 'portugal,viana do castelo, ponte de lima', 11-01-2020)).

% -------------------------------------------------------------------------------------------------
%   Conhecimento Interdito
% -------------------------------------------------------------------------------------------------

contrato(25, 21, 1, 'locacao de bens moveis', 'concurso publico', 'aquisicao de equipamentos', 12345, prazo_contrato_nulo, 'vila verde', 30-09-2019).
nulo(prazo_contrato_nulo).
excecao(contrato(A, B, C, D, E, F, G, H, I, J)) :- contrato(A, B, C, D, E, F, G, prazo_contrato_nulo, I, J).
