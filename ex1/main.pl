
% SICStus flags

:- set_prolog_flag( discontiguous_warnings,off ).
:- set_prolog_flag( single_var_warnings,off ).
:- set_prolog_flag( unknown,fail ).


% Extensão do predicado adjudicante: #IdAd, Nome, NIF, Morada -> {V, F, D}
adjudicante(1, 'Município de Alto de Basto', 705330336, 'Portugal,Braga, Alto de Basto').

% Extensão do predicado adjudicatária: #IdAda, Nome, NIF, Morada -> {V, F, D}
adjudicataria(1, 'XXX - Associados - Sociedade de Advogados, SP, RL.', 702675112, 'Portugal')

% Extensão do predicado contrato:
% #IdAda, TipoDeContrato, TipoDeProcedimento, Descrição, Custo, Prazo, Local, Data
%    -> {V, F, D}
contrato( 705330336, 702675112
        , 'Aquisição de serviços', 'Consulta Prévia'
        , 'Assessoria jurídica', 13599
        , 547, 'Alto de Basto', '11-02-2020').