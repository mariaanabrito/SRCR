%--------------------------------------------------------------------------------------------
%--------------------------------------------------------------------------------------------
% SIST. REPR. CONHECIMENTO E RACIOCINIO - MiEI/3

% TRABALHO DE GRUPO - 2 EXERCICIO

% GRUPO 2

%--------------------------------------------------------------------------------------------
%--------------------------------------------------------------------------------------------


%--------------------------------------------------------------------------------------------
%--------------------Declaracões Iniciais----------------------------------------------------
%--------------------------------------------------------------------------------------------

:- set_prolog_flag( discontiguous_warnings,off ).
:- set_prolog_flag( single_var_warnings,off ).
:- set_prolog_flag( unknown,fail ).

:- op(900,xfy,'::' ).
:- op(500, xfy, 'e').
:- op(400, xfy, 'ou').
:- dynamic '::'/2.
:- dynamic utente/4.
:- dynamic medico/4.
:- dynamic especialidade/2.
:- dynamic cuidado/4.
:- dynamic atomedico/9.
:- dynamic excecao/1.
:- dynamic nulo/1.
:- dynamic '-'/1.

%--------------------------------------------------------------------------------------------
%------Extensão do predicado que permite encontrar todas as soluções-------------------------
%--------------------------------------------------------------------------------------------

solucoes(X,Y,Z):-
      findall(X,Y,Z).

%--------------------------------------------------------------------------------------------
%------Extensão do predicado que permite a remocao do conhecimento: T -> {V,F}---------------
%--------------------------------------------------------------------------------------------


remocao(Termo) :- solucoes(Inv,-Termo::Inv,Linv),
                     remove(Termo),
                     testa(Linv).

remove(Termo) :- retract(Termo).
remove(Termo) :- assert(Termo).


%--------------------------------------------------------------------------------------------
%------Extensão do predicado que calcula os elementos de uma lista: T -> {V,F}---------------
%--------------------------------------------------------------------------------------------

comprimento(S,N) :- length(S,N).


%--------------------------------------------------------------------------------------------
%------Extensão do predicado que permite a evolucao do conhecimento perfeito: T -> {V,F}--------------
%--------------------------------------------------------------------------------------------

evolucao( Termo ) :- solucoes( Inv, +Termo::Inv, Linv),
                       assert(Termo),
                        testa(Linv).
evolucao( Termo ) :- retract(Termo), !, fail.

testa([]).
testa([H|T]):- H, testa(T).

%--------------------------------------------------------------------------------------------
%------Extensão do predicado que permite a evolucao do conhecimento incerto: T -> {V,F}--------------
%--------------------------------------------------------------------------------------------

regressaoMoradaIncerta(IdU, Nome, Idade) :-
                  removerUtente(IdU, Nome, Idade, morada_desconhecida),
                    remove((excecao(utente(Id, N, I, M)) :- utente(Id, N, I, morada_desconhecida))).


evolucaoMoradaIncerta(IdU, Nome, Idade) :-
                  evolucao(utente(IdU, Nome, Idade, morada_desconhecida)),
                    assert((excecao(utente(Id, N, I, M)) :- utente(Id, N, I, morada_desconhecida))).

regressaoEspecialidadeIncerta(IdS, Hosp, Local) :-
                  removerCuidado(IdS, esp_desconhecida, Hosp, Local),
                    remove((excecao(cuidado(IdSrv, IdE, H, L)) :- cuidado(IdSrv, esp_desconhecida, H, L))).


evolucaoEspecialidadeIncerta(IdS, Hosp, Local) :-
                  evolucao(cuidado(IdS, esp_desconhecida, Hosp, Local)),
                    assert((excecao(cuidado(IdSrv, IdE, H, L)) :- cuidado(IdSrv, esp_desconhecida, H, L))).



regressaoHorarioIncerto(Dia, Mes, Ano, IdU, IdS, IdMed, Custo) :-
              removerAtoMedico(hora_desconhecida, minutos_desconhecidos, Dia, Mes, Ano, IdU, IdS, IdMed, Custo),
                  remove((excecao(atomedico(H, Min, D, M, A, IdUt, IdSrv, IdM, C)) :-
                          atomedico(hora_desconhecida, minutos_desconhecidos, D, M, A, IdUt, IdSrv, IdM, C))).

evolucaoHorarioIncerto(Dia, Mes, Ano, IdU, IdS, IdMed, Custo) :-
              evolucao(atomedico(hora_desconhecida, minutos_desconhecidos, Dia, Mes, Ano, IdU, IdS, IdMed, Custo)),
                  assert((excecao(atomedico(H, Min, D, M, A, IdUt, IdSrv, IdM, C)) :-
                          atomedico(hora_desconhecida, minutos_desconhecidos, D, M, A, IdUt, IdSrv, IdM, C))).


%--------------------------------------------------------------------------------------------
%--------------------CONHECIMENTO PERFEITO---------------------------------------------------
%--------------------------------------------------------------------------------------------

%--------------------------------------------------------------------------------------------
%------Utente: #IdUt, Nome, Idade, Morada -> {V,F}-------------------------------------------
%--------------------------------------------------------------------------------------------

utente(u1,'Pedro Pires',21,'Braga').
utente(u2,'Marta Barros',29,'Braga').
utente(u3,'Guilherme Martins',36,'Guimaraes').
utente(u8, 'Joana Monteiro', 12, 'Algarve').
utente(u9, 'Manuel Santidade', 23, 'Palmela').

%--------------------------------------------------------------------------------------------
%------Médico : #IdMed, Nome, Idade, #IdEsp -> {V,F}-----------------------------------------
%--------------------------------------------------------------------------------------------

medico(m1, 'Fernando Fernandes', 34, e1).
medico(m2, 'Paulo Pinho', 48, e2).
medico(m3, 'Augusto Agostinho', 52, e3).
medico(m4, 'Bernardo Barros', 44, e4).
medico(m5, 'Carlos Costa', 45, e5).

%--------------------------------------------------------------------------------------------
%------Especialidade : #IdEspecialidade, Designação------------------------------------------
%--------------------------------------------------------------------------------------------


especialidade(e1, 'geral').
especialidade(e2, 'pediatria').
especialidade(e3, 'urologia').
especialidade(e4, 'ginecologia').
especialidade(e5, 'psiquiatria').
especialidade(e6, 'radiologia').


%--------------------------------------------------------------------------------------------
%------Cuidado prestado: #IdServ, Descrição, Instituição, Cidade -> {V,F}--------------------
%--------------------------------------------------------------------------------------------

cuidado(s1,e5,'hospital sao joao','Porto').
cuidado(s2,e4,'hospital sao marcos','Braga').
cuidado(s3,e2,'hospital sao joao','Porto').
cuidado(s4,e1,'centro de saude do caranda','Braga').
cuidado(s5,e3,'hospital sao joao','Porto').
cuidado(s6,e2,'maternidade alfredo da costa','Lisboa').


%-----------------------------------------------------------------------------------------------------------------
%------Ato medico: Hora,Minutos , Dias, Mes, Ano, #IdUt, #IdServ, #IdMed, Custo(€€.€€) -> {V,F}----------
%-----------------------------------------------------------------------------------------------------------------


atomedico(12, 20, 10, 3, 2017, u1, s4, m1, 12.20).
atomedico(21, 00, 2, 3, 2017, u2, s3, m2, 16.50).
atomedico(09, 40, 14, 2, 2017, u3, s5, m3, 21.00).

%--------------------------------------------------------------------------------------------
%------Registo de Utente: #IdUt, Nome, Idade, Morada) -> {V,F}-------------------------------
%--------------------------------------------------------------------------------------------

registoUtente(Id,Nome,Idade,Morada) :-
            evolucao(utente(Id,Nome,Idade,Morada)),
              verificaMoradaIncerta(utente(Id, Nome, Idade, Morada)),
                apagaExcecoesIdade(utente(Id, Nome, Idade, Morada)).


verificaMoradaIncerta(utente(IdU, Nome, Idade, Morada)) :- 
            removeMoradaIncerta(utente(IdU, Nome, Idade, Morada)).


removeMoradaIncerta(utente(IdU, Nome, Idade, Morada)) :- demo(utente(IdU, Nome, Idade, morada_desconhecida), verdadeiro),
                                                             removerUtente(IdU, Nome, Idade, morada_desconhecida),
                                                             remove((excecao(utente(Id, N, I, M)) :-
                                                              utente(Id, N, I, morada_desconhecida))).

removeMoradaIncerta(utente(IdU, Nome, Idade, Morada)).

apagaExcecoesIdade(utente(IdU, Nome, Idade, Morada)) :- 
              demo(excecao(utente(IdU, Nome, _, Morada)), verdadeiro),
              solucoes(utente(IdU, Nome, I, Morada), excecao(utente(IdU,Nome,_,Morada)),S),
              regressaoExcecao(S).

apagaExcecoesIdade(utente(IdU, Nome, Idade, Morada)).

%--------------------------------------------------------------------------------------------
%------Registo de Médico: #IdMed, Nome, Idade, IdEsp) -> {V,F}-------------------------------
%--------------------------------------------------------------------------------------------

registoMedico(IdMed,Nome,Idade,IdEsp) :-
            evolucao(medico(IdMed,Nome,Idade,IdEsp)).


%--------------------------------------------------------------------------------------------
%------Registo de Especialidade: #IdEspecialidade, Designação -> {V,F}-----------------------
%--------------------------------------------------------------------------------------------

registoEspecialidade(IdEsp,Desig) :-
            evolucao(especialidade(IdEsp,Desig)).


%--------------------------------------------------------------------------------------------
%------Registo de Cuidado: #IdCuidado,Descricao,Instituicao,Cidade -> {V,F}------------------
%--------------------------------------------------------------------------------------------

registoCuidado(Id,Descricao,Instituicao,Cidade) :-
            evolucao(cuidado(Id,Descricao,Instituicao,Cidade)),
            removeEspecialidadeIncerta(cuidado(Id, Descricao, Instituicao, Cidade)).


removeEspecialidadeIncerta(cuidado(Id, Descricao, Instituicao, Cidade)) :-
              demo(cuidado(Id, esp_desconhecida, Instituicao, Cidade), verdadeiro),
                 removerCuidado(Id, esp_desconhecida, Instituicao, Cidade),
                    retract((excecao(cuidado(IdS, D, I, C)) :-
                        cuidado(IdS, esp_desconhecida, I, C))).
                  

removeEspecialidadeIncerta(cuidado(Id, Descricao, Instituicao, Cidade)).


%-------------------------------------------------------------------------------------------------------
%------Registo de atos medicos: Hora, Data, Id-Utilizador, Id-Servico, Id-Medico, Custo -> {V,F}--------
%-------------------------------------------------------------------------------------------------------

registoAtoMedico(Hora, Minutos, Dia, Mes, Ano, IdU, IdS, IdMed, Custo) :-
            evolucao(atomedico(Hora, Minutos, Dia, Mes, Ano, IdU, IdS, IdMed, Custo)),
              removeHorarioIncerto(atomedico(Hora, Minutos, Dia, Mes, Ano, IdU, IdS, IdMed, Custo)),
                apagaExcecoesCusto(atomedico(Hora, Minutos, Dia, Mes, Ano, IdU, IdS, IdMed, Custo)).


removeHorarioIncerto(atomedico(Hora, Minutos, Dia, Mes, Ano, IdU, IdS, IdMed, Custo)) :-
              demo(atomedico(hora_desconhecida, minutos_desconhecidos, Dia, Mes, Ano, IdU, IdS, IdMed, Custo), verdadeiro),
                 removerAtoMedico(hora_desconhecida, minutos_desconhecidos, Dia, Mes, Ano, IdU, IdS, IdMed, Custo),
                    remocao((excecao(atomedico(H, Min, D, M, A, IdUt, IdSrv, IdM, C)) :-
                        atomedico(hora_desconhecida, minutos_desconhecidos, D, M, A, IdUt, IdSrv, IdM, C))).
                 
removeHorarioIncerto(atomedico(Hora, Minutos, Dia, Mes, Ano, IdU, IdS, IdMed, Custo)).


apagaExcecoesCusto(atomedico(Hora, Minutos, Dia, Mes, Ano, IdU, IdS, IdMed, Custo)) :-
        demo(atomedico(Hora, Minutos, Dia, Mes, Ano, IdU, IdS, IdMed, _), verdadeiro),
          solucoes(atomedico(Hora, Minutos, Dia, Mes, Ano, IdU, IdS, IdMed, C), excecao(atomedico(Hora, Minutos, Dia, Mes, Ano, IdU, IdS, IdMed, _)),S),
            regressaoExcecao(S).
            
apagaExcecoesCusto(atomedico(Hora, Minutos, Dia, Mes, Ano, IdU, IdS, IdMed, Custo)).

%--------------------------------------------------------------------------------------------
%------Predicado que remove um utente--------------------------------------------------------
%--------------------------------------------------------------------------------------------

removerUtente(Id,Nome,Idade,Morada) :-
        remocao(utente(Id,Nome,Idade,Morada )).


%--------------------------------------------------------------------------------------------
%-------Predicado que remove um medico-------------------------------------------------------
%--------------------------------------------------------------------------------------------

removerMedico(IdMed, Nome, Idade, IdEsp) :-
        remocao(medico(IdMed, Nome, Idade, IdEsp)).


%--------------------------------------------------------------------------------------------
%------Predicado que remove uma especialidade------------------------------------------------
%--------------------------------------------------------------------------------------------

removerEspecialidade(IdEsp, Desig) :-
        remocao(especialidade(IdEsp, Desig)).

%--------------------------------------------------------------------------------------------
%------ Predicado que remove um ato médico --------------------------------------------------
%--------------------------------------------------------------------------------------------

removerAtoMedico(Hora, Minutos, Dia, Mes, Ano, IdU, IdS, IdMed, Custo) :-
        remocao(atomedico(Hora, Minutos, Dia, Mes, Ano, IdU, IdS, IdMed, Custo)).

%--------------------------------------------------------------------------------------------
%------Predicado que remove um cuidado-------------------------------------------------------
%--------------------------------------------------------------------------------------------

removerCuidado(Id,Descricao,Instituicao,Cidade) :-
        remocao(cuidado(Id,Descricao,Instituicao,Cidade)).


% ---------------------------------------------------------------------------------------------------------
%---------------------------------- INVARIANTES -----------------------------------------------------------
% ---------------------------------------------------------------------------------------------------------

% --------------- Invariantes de Inserção ---------------------------------------

%--------------------------------------------------------------------------------
%------Invariante que não permite a inserção de utentes com o mesmo ID, Nome, Idade e Morada-----------
%--------------------------------------------------------------------------------

+utente(Idu,Nome,Idade,Morada) :: (solucoes(Idu,utente(Idu,Nome,Idade,Morada),S),
                                  comprimento(S,N),
                                  N == 1).

%--------------------------------------------------------------------------------
%------Invariante que não  permite a inserção de médicos com o mesmo ID----------
%--------------------------------------------------------------------------------

+medico(IdMed,Nome,Idade,IdEsp) :: (solucoes(IdMed,medico(IdMed,_,_,_),S),
                                   comprimento(S,N),
                                   N == 1).


%--------------------------------------------------------------------------------
%------Invariante que não permite a inserção de especialidades com o mesmo ID----
%--------------------------------------------------------------------------------

+especialidade(IdEsp, Desig) :: (solucoes(IdEsp,especialidade(IdEsp,_),S),
                                   comprimento(S,N),
                                   N == 1).


%---------------------------------------------------------------------------------------------------------------------
%------Invariante que não permite a inserção de um ato médico em que o médico já tenha consulta à hora estipulada-----
%---------------------------------------------------------------------------------------------------------------------

+atomedico(Hora, Minutos,Dia, Mes, Ano,IdU,IdS,IdMed,Custo) :: (solucoes(IdMed, atomedico(Hora,Minutos, Dia, Mes, Ano,_,_,IdMed,_),S),
                                             comprimento(S,N),
                                             N==1).


%-----------------------------------------------------------------------------------------------------------------
%------Invariante que não permite a inserção de um ato médico à mesma hora, na mesma data com o mesmo utente------
%-----------------------------------------------------------------------------------------------------------------

+atomedico(Hora, Minutos,Dia, Mes, Ano,IdU,IdS,IdMed,Custo) :: (solucoes(IdU, atomedico(Hora, Minuto, Dia, Mes, Ano,IdU,_,_,_),S),
                                              comprimento(S,N),
                                              N == 1).



%--------------------------------------------------------------------------------------------
%-----------------------------FASE 2---------------------------------------------------------
%--------------------------------------------------------------------------------------------


%--------------------------------------------------------------------------------------------
%-----------------------------Extensao do meta-predicado nao: Questao -> {V,F}---------------
%--------------------------------------------------------------------------------------------

nao( Questao ) :-
    Questao, !, fail.
nao( Questao ).

%--------------------------------------------------------------------------------------------
%--------------Extensao do meta-predicado demo: Questao,Resposta -> {V,F,D}--------------------
%--------------------------------------------------------------------------------------------

demo( Questao,verdadeiro ) :-
    Questao.
demo( Questao, falso ) :-
    -Questao.
demo( Questao,desconhecido ) :-
    nao( Questao ),
    nao( -Questao ).


%--------------------------------------------------------------------------------------------
%---------Extensão do meta-predicado demoListas: [Questao],[Resposta] -> {V,F,D}-------------
%--------------------------------------------------------------------------------------------

demoListas([], []).
demoListas([H|T], R) :-
        demoThreeValued(H, A),
        demoListas(T, B),
        R = [A|B].

%--------------------------------------------------------------------------------------------
%---------Extensão do meta-predicado demoThreeValued.---------------------------------------
%--------------------------------------------------------------------------------------------

demoThreeValued((Q1 e Q2), verdadeiro) :- demo(Q1, verdadeiro), demo(Q2,verdadeiro).

demoThreeValued((Q1 e Q2), falso) :- demo(Q1, falso), demo(Q2,verdadeiro).
demoThreeValued((Q1 e Q2), falso) :- demo(Q1, verdadeiro), demo(Q2,falso).
demoThreeValued((Q1 e Q2), falso) :- demo(Q1, desconhecido), demo(Q2,falso).
demoThreeValued((Q1 e Q2), falso) :- demo(Q1, falso), demo(Q2,desconhecido).
demoThreeValued((Q1 e Q2), falso) :- demo(Q1, falso), demo(Q2,falso).

demoThreeValued((Q1 e Q2), desconhecido) :- demo(Q1, desconhecido), demo(Q2,desconhecido).
demoThreeValued((Q1 e Q2), desconhecido) :- demo(Q1, desconhecido), demo(Q2,verdadeiro).
demoThreeValued((Q1 e Q2), desconhecido) :- demo(Q1, verdadeiro), demo(Q2,desconhecido).

demoThreeValued((Q1 ou Q2), verdadeiro) :- demo(Q1, verdadeiro), demo(Q2, verdadeiro).
demoThreeValued((Q1 ou Q2), verdadeiro) :- demo(Q1, verdadeiro), demo(Q2, desconhecido).
demoThreeValued((Q1 ou Q2), verdadeiro) :- demo(Q1, desconhecido), demo(Q2, verdadeiro).
demoThreeValued((Q1 ou Q2), verdadeiro) :- demo(Q1, verdadeiro), demo(Q2, falso).
demoThreeValued((Q1 ou Q2), verdadeiro) :- demo(Q1, falso), demo(Q2, verdadeiro).

demoThreeValued((Q1 ou Q2), falso) :- demo(Q1, falso), demo(Q2, falso).

demoThreeValued((Q1 ou Q2), desconhecido) :- demo(Q1, desconhecido), demo(Q2, desconhecido).
demoThreeValued((Q1 ou Q2), desconhecido) :- demo(Q1, falso), demo(Q2, desconhecido).
demoThreeValued((Q1 ou Q2), desconhecido) :- demo(Q1, desconhecido), demo(Q2, falso).


%--------------------------------------------------------------------------------------------
%-----------------------------Conhecimento Negativo------------------------------------------
%--------------------------------------------------------------------------------------------

%Invariante que não permite adicionar conhecimento negativo repetido
+(-Termo) :: (solucoes(Termo, -Termo, S),
                 comprimento(S,N),
                   N =< 2).

%Invariante que não deixa adicionar conhecimento positivo que contradiz o conhecimento negativo
+Termo :: nao(-Termo).

%Invariante que não deixa adicionar conhecimento negativo que contradiz o conhecimento positivo
+(-Termo) :: nao(Termo).

evolucaoUtenteNeg(IdU, Nome, Idade, Morada) :- evolucao(-utente(IdU, Nome, Idade, Morada)).

evolucaoCuidadoNeg(IdS, IdEsp, Hosp, Local) :- evolucao(-cuidado(IdS, IdEsp, Hosp, Local)).

evolucaoAtoMedicoNeg(Hora, Minutos, Dia, Mes, Ano, IdU, IdS, IdMed, Custo) :-
                  evolucao(-atomedico(Hora, Minutos, Dia, Mes, Ano, IdU, IdS, IdMed, Custo)).


regressaoUtenteNeg(IdU, Nome, Idade, Morada) :- remove(-utente(IdU, Nome, Idade, Morada)).

regressaoCuidadoNeg(IdS, IdEsp, Hosp, Local) :- remove(-cuidado(IdS, IdEsp, Hosp, Local)).

regressaoAtoMedicoNeg(Hora, Minutos, Dia, Mes, Ano, IdU, IdS, IdMed, Custo) :-
                  remove(-atomedico(Hora, Minutos, Dia, Mes, Ano, IdU, IdS, IdMed, Custo)).

% Conhecimento negativo do utente

-utente(u4, 'Rita Sousa', 70, 'Faro').

%Conhecimento negativo do cuidado

-cuidado(s11,e6,'hospital sao joao','Porto').

%Conhecimento negativo do ato médico

-atomedico(14,20,14,2,2017,u2,s1,m5,40.90).


% Predicados para conhecimento negativo perfeito

-medico(IdMed, Nome, Idade, Esp) :-nao(medico(IdMed, Nome, Idade, Esp)),
                                    nao(excecao(medico(IdMed, Nome, Idade, Esp))).

-especialidade(IdEsp, Desc) :- nao(especialidade(IdEsp, Desc)),
                                  nao(excecao(especialidade(IdEsp, Desc))).

-utente(IdU, Nome, Idade, Morada) :- nao(utente(IdU, Nome, Idade, Morada)),
                                      nao(excecao(utente(IdU, Nome, Idade, Morada))).

-cuidado(IdS, IdE, Hosp, Local) :- nao(cuidado(IdS, IdE, Hosp, Local)),
                                      nao(excecao(cuidado(IdS, IdE, Hosp, Local))).

-atomedico(Hora, Minutos,Dia, Mes, Ano,IdU,IdS,IdMed,Custo) :- nao(atomedico(Hora, Minutos,Dia, Mes, Ano,IdU,IdS,IdMed,Custo)),
                                              nao(excecao(atomedico(Hora, Minutos,Dia, Mes, Ano,IdU,IdS,IdMed,Custo))).

%--------------------------------------------------------------------------------------------
%-----------------------------Conhecimento Incerto------------------------------------------
%--------------------------------------------------------------------------------------------

% Conhecimento incerto sobre utente

utente(u7, 'John McCarthy', 56, morada_desconhecida).

excecao(utente(IdU, Nome, Idade, Morada)) :- utente(IdU, Nome, Idade, morada_desconhecida).

% Conhecimento incerto sobre cuidado

cuidado(s10, esp_desconhecida, 'centro de saude de infias', 'Braga').

excecao(cuidado(IdS, IdE, Hosp, Local)) :- cuidado(IdS, esp_desconhecida, Hosp, Local).


% Conhecimento incerto sobre ato médico

% Não se sabe o horário da consulta

atomedico(hora_desconhecida, minutos_desconhecidos, 10, 7, 2018, u3, s5, m3, 143.90).

excecao(atomedico(Hora, Minutos, Dia, Mes, Ano, IdU, IdS, IdMed, Custo)) :-
              atomedico(hora_desconhecida, minutos_desconhecidos,Dia, Mes, Ano, IdU, IdS, IdMed, Custo).


%--------------------------------------------------------------------------------------------
%-----------------------------Conhecimento Impreciso------------------------------------------
%--------------------------------------------------------------------------------------------


% Evolução para controlar conhecimento impreciso sobre o utente (por ex., idade é um valor ou outro)

regressaoExcecao([]).
regressaoExcecao([Termo|R]) :- remove(excecao(Termo)),
                                regressaoExcecao(R).


evolucaoUtenteImpreciso([]).
evolucaoUtenteImpreciso([Termo|T]) :-
                  assert(excecao(Termo)),
                    evolucaoUtenteImpreciso(T).

excecao(utente(u10, 'Diana Dias', 25, 'Viseu')).
excecao(utente(u10, 'Diana Dias', 26, 'Viseu')).


% Evolução para controlar conhecimento impreciso sobre o ato médico (custo entre dois valores)

evolucaoCustoImpreciso(H, Min, Dia, Mes, Ano, IdU, IdS, IdMed, LI, LS) :-
          assert((excecao(atomedico(H, Min, Dia, Mes, Ano, IdU, IdS, IdMed, Y)) :- Y >= LI, Y =< LS)).

excecao(atomedico(18, 20, 23, 11, 2018, u9, s1, m5, X)) :- X >= 30, X =< 40.

% Evolução para controlar conhecimento impreciso sobre o ato médico (por ex., custo é um valor ou outro)


evolucaoCustoDiscImpreciso([]).
evolucaoCustoDiscImpreciso([Termo|T]) :-
                  assert(excecao(Termo)),
                    evolucaoCustoDiscImpreciso(T).

% O preço de uma consulta pode ser 10 ou 100
excecao(atomedico(13,50, 17,10, 2019, u8, s2, m4, 10)).
excecao(atomedico(13,50, 17,10, 2019, u8, s2, m4, 100)).

% Evolução para controlar conhecimento impreciso sobre o ato médico (dia entre dois valores)

evolucaoDiaImpreciso(H, Min, Mes, Ano, IdU, IdS, IdMed, Custo, LI, LS) :-
                  assert((excecao(atomedico(H, Min, X, Mes, Ano, IdU, IdS, IdMed, Custo)) :- X >= LI, X =< LS)).


%--------------------------------------------------------------------------------------------
%-----------------------------Conhecimento Interdito------------------------------------------
%--------------------------------------------------------------------------------------------

nulo(morada_interdita).
nulo(esp_interdita).
nulo(custo_interdito).

% Evolução sobre utente com morada interdita.

regressaoMoradaInterdita(IdU, Nome, Idade) :-
                removerUtente(IdU, Nome, Idade, morada_interdita),
                  remove(+utente(Id, N, I, Morada) ::
                    (solucoes(M, (utente(IdU, N, I, M), nao(nulo(M))), S),
                        comprimento(S,C),
                        C==0)),
                  remove((excecao(utente(Id, N, I, M)) :- utente(Id, N, I, morada_interdita))),
                    remove(nulo(morada_interdita)).


evolucaoMoradaInterdita(IdU, Nome, Idade) :-
                evolucao(utente(IdU, Nome, Idade, morada_interdita)),
                  assert(+utente(Id, N, I, Morada) ::
                    (solucoes(M, (utente(IdU, N, I, M), nao(nulo(M))), S),
                        comprimento(S,C),
                        C==0)),
                  assert((excecao(utente(Id, N, I, M)) :- utente(Id, N, I, morada_interdita))),
                    assert(nulo(morada_interdita)).

% Utente com morada interdita

utente(u12, 'Cristiano Ronaldo', 33, morada_interdita).

excecao(utente(IdU, Nome, Idade, Morada)) :-
                utente(IdU, Nome, Idade, morada_interdita).

% Invariante que não permite a inserção de conhecimento positivo quando há conhecimento interdito

+utente(IdU, Nome, Idade, Morada) ::
    (solucoes(M, (utente(u12, Nome, Idade, M), nao(nulo(M))), S),
          comprimento(S,N),
          N==0).



%Evolução sobre o médico com especialidade interdita

regressaoEspecialidadeInterdita(IdMed, Nome, Idade) :-
                          removerMedico(IdMed, Nome, Idade, esp_interdita),
                              remove((excecao(medico(IdM, N, I, E))
                                    :- medico(IdM, N, I, esp_interdita))),
                                remove(+medico(Id, N, I, Especialidade) ::
                                  (solucoes(E, (medico(IdMed, N, I, E), nao(nulo(E))), S),
                                        comprimento(S,C),
                                        C==0)),
                                remove(nulo(esp_interdita)).


evolucaoEspecialidadeInterdita(IdMed, Nome, Idade) :-
                          evolucao(medico(IdMed, Nome, Idade, esp_interdita)),
                              assert((excecao(medico(IdM, N, I, E))
                                    :- medico(IdM, N, I, esp_interdita))),
                                assert(+medico(Id, N, I, Especialidade) ::
                                  (solucoes(E, (medico(IdMed, N, I, E), nao(nulo(E))), S),
                                        comprimento(S,C),
                                        C==0)),
                                assert(nulo(esp_interdita)).

medico(m6, 'Rodrigo Silva', 53, esp_interdita).

excecao(medico(IdMed, Nome, Idade, Esp)) :-
                  medico(IdMed, Nome, Idade, esp_interdita).


+medico(IdU, Nome, Idade, Especialidade) ::
    (solucoes(E, (medico(m6, Nome, Idade, E), nao(nulo(E))), S),
          comprimento(S,N),
          N==0).


% Não se pode conhecer o custo de uma consulta

regressaoCustoInterdito(Hora, Minutos, Dia, Mes, Ano, IdU, IdS, IdMed) :-
                          removerAtoMedico(Hora, Minutos, Dia, Mes, Ano, IdU, IdS, IdMed, custo_interdito),
                              remove((excecao(atomedico(H, Min, D, M, A, IdUt, IdSrv, IdM, Ct)) :-
                                atomedico(Hora, Minutos, Dia, Mes, Ano, IdU, IdS, IdMed, custo_interdito))),
                                remove(+atomedico(H, Min, D, M, A, IdUt, IdSrv, IdM, Ct) ::
                                  (solucoes(C, (atomedico(Hora, Minutos, Dia, Mes, Ano, IdU, IdS, IdMed, C), nao(nulo(C))), S),
                                        comprimento(S,L),
                                        L==0)),
                                  remove(nulo(custo_interdito)).


evolucaoCustoInterdito(Hora, Minutos, Dia, Mes, Ano, IdU, IdS, IdMed) :-
                          evolucao(atomedico(Hora, Minutos, Dia, Mes, Ano, IdU, IdS, IdMed, custo_interdito)),
                              assert((excecao(atomedico(H, Min,D, M, A, IdUt, IdSrv, IdM, C))
                                    :- atomedico(H, Min,D, M, A, IdUt, IdSrv, IdM, custo_interdito))),
                                assert(+atomedico(H, Min, D, M, A, IdUt, IdSrv, IdM, Ct) ::
                                  (solucoes(C, (atomedico(Hora, Minutos, Dia, Mes, Ano, IdU, IdS, IdMed, C), nao(nulo(C))), S),
                                        comprimento(S,L),
                                        L==0)),
                                  assert(nulo(custo_interdito)).

atomedico(16, 45, 9, 10, 2017, u1, s5, m3, custo_interdito).

excecao(atomedico(Hora, Minutos,Dia, Mes, Ano,IdU,IdS,IdMed,Custo)) :-
                  atomedico(Hora,Minutos,Dia,Mes,Ano,IdU,IdS, IdMed, custo_interdito).

+atomedico(Hora, Minutos, Dia, Mes, Ano, IdU, IdS, IdMed, Custo) ::
    (solucoes(C, (atomedico(16, 45, 9, 10, 2017, u1, s5, m3, C), nao(nulo(C))), S),
          comprimento(S,N),
          N==0).
