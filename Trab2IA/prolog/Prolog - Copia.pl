%Fatos

:-dynamic([
      inimigoW/2,
      poco/1,
      ouro/1,
	  up/1,
	  teletransporter/1,
      inimigoD/2,
      estado/6,
      tem_cheiroW/1,
	  tem_flash/1,
      tem_vento/1,
      tem_cheiroD/1,
      percorrido/1,
      acoes/1,
	  while/2,
	  tamanholista/2,
	  subtractMin/4,
	  subtractMax/4
	  
  ]).

tamanho_mapa(12). 
 
inimigoD(casa(9,3),100). 
inimigoD(casa(4,9),100).

inimigoW(casa(3,3),100).
inimigoW(casa(6,8),100).

poco(casa(10,1)).
poco(casa(2,2)).
%poco(casa(11,4)).
poco(casa(7,6)).
poco(casa(8,8)).
poco(casa(10,9)).
poco(casa(2,10)).
poco(casa(5,11)).

ouro(casa(1,1)).

ouro(casa(7,8)).

ouro(casa(11,12)).

teletransporter(casa(2,6)).
teletransporter(casa(5,7)).
teletransporter(casa(12,7)).
teletransporter(casa(9,11)).

up(casa(6,1)).
up(casa(3,7)). 
up(casa(12,10)).

estado(casa(12,1),norte,0,100,0,vivo).
percorrido(casa(12,1)).
acoes([]).

%Liston([1,2,3,4,5,6,7,8,9,10,11,12]).

tamanholista([],0).
tamanholista([_| R],N) :- tamanholista(R, N1), N is N1+1.

%Regras


%Utilidades

adjacente(casa(X,Y),casa(W,Z)) :-  (W is X+1,Z = Y);(W is X-1,Z = Y);(W = X,Z is Y+1);(W = X,Z is Y-1),  (W > 0, W< 13), (Z >0, Z<13) .

cheiroW(casa(X,Y)) :- inimigoW(casa(W,Z),_), adjacente(casa(X,Y), casa(W,Z)).

vento(casa(X,Y)) :- poco(casa(W,Z)), adjacente(casa(X,Y), casa(W,Z)).

cheiroD(casa(X,Y)) :- inimigoD(casa(W,Z),_), adjacente(casa(X,Y), casa(W,Z)).

flash(casa(X,Y)) :- teletransporter(casa(W,Z)), adjacente(casa(X,Y), casa(W,Z)).

brilho(casa(X,Y)) :- ouro(casa(X,Y)).

%Ações
%/////////////////

virar_direita:- retract(estado(casa(Linha,Coluna),norte,Pontos,Vida,Ouros,Situacao)),NovoPontos is Pontos -1,
assert(estado(casa(Linha,Coluna),leste,NovoPontos,Vida,Ouros,Situacao)),!.

virar_direita:- retract(estado(casa(Linha,Coluna),sul,Pontos,Vida,Ouros,Situacao)), NovoPontos is Pontos -1,
assert(estado(casa(Linha,Coluna),oeste,NovoPontos,Vida,Ouros,Situacao)),!.

virar_direita:- retract(estado(casa(Linha,Coluna),leste,Pontos,Vida,Ouros,Situacao)), NovoPontos is Pontos -1,
assert(estado(casa(Linha,Coluna),sul,NovoPontos,Vida,Ouros,Situacao)),!.

virar_direita:- retract(estado(casa(Linha,Coluna), oeste,Pontos,Vida,Ouros,Situacao)), NovoPontos is Pontos -1,
assert(estado(casa(Linha,Coluna),norte,NovoPontos,Vida,Ouros,Situacao)),!.


%Fim_Ações
%/////////////////


%Checar_Andar - FIM
%/////////////////


%Andar
%/////////////////

andar:- estado(casa(Lin,_),norte,Pont,Vida,Our,Sit), Lin > 1,
retract(estado(casa(Lin,Col),Ori,Pont,Vida,Our,Sit)), 
NovoPont is Pont -1,
NovaLin is Lin - 1,
assert(estado(casa(NovaLin,Col),Ori,NovoPont,Vida,Our,Sit)),retract(acoes(Lista)), assert(acoes([andar_baixo|Lista])),
perceber_sofrer,!.

andar:- estado(casa(_,Col),leste,Pont,Vida,Our,Sit), Col < 12,
retract(estado(casa(Lin,Col),Ori,Pont,Vida,Our,Sit)), 
NovoPont is Pont -1,
NovaCol is Col + 1,
assert(estado(casa(Lin,NovaCol),Ori,NovoPont,Vida,Our,Sit)),retract(acoes(Lista)), assert(acoes([andar_esquerda|Lista])),
perceber_sofrer,!.

andar:- estado(casa(Lin,_),sul,Pont,Vida,Our,Sit),  Lin < 12,
retract(estado(casa(Lin,Col),Ori,Pont,Vida,Our,Sit)),
NovoPont is Pont -1,
NovaLin is Lin + 1,
assert(estado(casa(NovaLin,Col),Ori,NovoPont,Vida,Our,Sit)),retract(acoes(Lista)), assert(acoes([andar_cima|Lista])),
perceber_sofrer,!.

andar:- estado(casa(_,Col),oeste,Pont,Vida,Our,Sit),  Col > 1,
retract(estado(casa(Lin,Col),Ori,Pont,Vida,Our,Sit)),
NovoPont is Pont -1,
NovaCol is Col - 1,
assert(estado(casa(Lin,NovaCol),Ori,NovoPont,Vida,Our,Sit)),retract(acoes(Lista)), assert(acoes([andar_direita|Lista])),
perceber_sofrer,!.

%Andar - FIM
%/////////////////

%Outras Ações
%/////////////////

%Atirar_Flecha 
atirar_flecha :- 
retract(estado(casa(Lin,Col),Orientacao,Pontos,Vida,Ouros,Situacao)),
NovoPontos is Pontos - 10,
assert(estado(casa(Lin,Col),Orientacao,NovoPontos,Vida,Ouros,Situacao)),
random_between(20,50,ValorFlecha), (inimigoD(casa(Lin,Col),VidaW);inimigoW(casa(Lin,Col),VidaW)), NovaVida is VidaW - ValorFlecha, NovaVida =< 0,
(retract(inimigoD(casa(Lin,Col),VidaW));retract(inimigoW(casa(Lin,Col),VidaW))), write("grito").

atirar_flecha :- 
retract(estado(casa(Lin,Col),Orientacao,Pontos,Vida,Ouros,Situacao)),
NovoPontos is Pontos - 10,
assert(estado(casa(Lin,Col),Orientacao,NovoPontos,Vida,Ouros,Situacao)),
random_between(20,50,ValorFlecha), (inimigoD(casa(Lin,Col),VidaW);inimigoW(casa(Lin,Col),VidaW)), NovaVida is VidaW - ValorFlecha, NovaVida > 0,
((retract(inimigoD(casa(Lin,Col),VidaW)),assert(inimigoD(casal(Lin,Col),NovaVida)));(retract(inimigoW(casa(Lin,Col),VidaW)),assert(inimigoW(casa(Lin,Col),NovaVida)))),perceber_sofrer,!.

%Pegar_Ouro
pegar_ouro :- retract(estado(casa(Lin,Col),Orientacao,Pontos,Vida,Ouros,Situacao)),
NovoOuros is Ouros + 1,
NovoPontos is Pontos + 1000,
retract(ouro(casa(Lin,Col))),
assert(estado(casa(Lin,Col),Orientacao,NovoPontos,Vida,NovoOuros,Situacao)),
((NovoOuros = 3, acoes(Lista), voltar(Lista)); true).

%Pegar_Vida
pegar_vida :- retract(estado(casa(Lin,Col),Orientacao,Vida,Pontos,Ouros,Situacao)),
NovaVida is Vida + 50, ((Vida > 100, NovaVida is 100);true),
retract(up(casa(Lin,Col))),
assert(estado(casa(Lin,Col),Orientacao,Pontos,NovaVida,Ouros,Situacao)).

%Teleporte
teleporte :- tamanho_mapa(Max), random_between(1,Max,RandLin),random_between(1,Max,RandCol), retract(estado(_,Ori,Pont,Vida,Our,Sit)),
assert(estado(casa(RandLin,RandCol),Ori,Pont,Vida,Our,Sit)), perceber_sofrer.

%Outras Ações - FIM
%/////////////////

%Percepções
%/////////////////

%Perceber_Sofrer
perceber_sofrer :- estado(casa(Lin,Col),_,_,Vida,_,_), assert(percorrido(casa(Lin,Col))), 
(
(cheiroW(casa(Lin,Col)), assert(tem_cheiroW(casa(Lin,Col))), false);
(vento(casa(Lin,Col)), assert(tem_vento(casa(Lin,Col))),false);
(flash(casa(Lin,Col)), assert(tem_flash(casa(Lin,Col))),false);
(cheiroD(casa(Lin,Col)), assert(tem_cheiroD(casa(Lin,Col))),false);
(brilho(casa(Lin,Col)), assert(tem_brilho(casa(Lin,Col))),false);
(inimigoD(casa(Lin,Col),_), NovaVida is Vida - 20, NovaVida > 0, retract(estado(casa(Linha, Coluna),Orientacao,Pontos,Vida,Ouros,vivo)), assert(estado(casa(Linha,Coluna),Orientacao,Pontos,NovaVida,Ouros,vivo)), false); 
(inimigoD(casa(Lin,Col),_), NovaVida is Vida - 20, NovaVida =< 0, retract(estado(casa(Linha, Coluna),Orientacao,Pontos,Vida,Ouros,_)), assert(estado(casa(Linha,Coluna),Orientacao,Pontos,NovaVida,Ouros,morto)), false);   
(inimigoW(casa(Lin,Col),_), NovaVida is Vida - 50, NovaVida > 0, retract(estado(casa(Linha, Coluna),Orientacao,Pontos,Vida,Ouros,vivo)), assert(estado(casa(Linha,Coluna),Orientacao,Pontos,NovaVida,Ouros,vivo)), false); 
(inimigoW(casa(Lin,Col),_), NovaVida is Vida - 50, NovaVida =< 0, retract(estado(casa(Linha, Coluna),Orientacao,Pontos,Vida,Ouros,_)), assert(estado(casa(Linha,Coluna),Orientacao,Pontos,NovaVida,Ouros,morto)), false); 
(poco(casa(Lin,Col)), retract(estado(casa(Linha, Coluna),Orientacao,Pontos,Vida,Ouros,_)), NovoPontos is Pontos - 1000, NovaVida is 0, assert(estado(casa(Linha,Coluna),Orientacao,NovoPontos,NovaVida,Ouros,morto)), false);
(teletransporter(casa(Lin,Col)), teleporte);
true
).

%Nao_InimigoW
nao_iniW(casa(Lin,Col)) :- percorrido(casa(X,Y)), adjacente(casa(Lin,Col), casa(X,Y)), not(tem_cheiroW(casa(X,Y))).
%Nao_Poco
nao_poco(casa(Lin,Col)) :- percorrido(casa(X,Y)), adjacente(casa(Lin,Col), casa(X,Y)), not(tem_vento(casa(X,Y))).
%Nao_Flash
nao_flash(casa(Lin,Col)) :- percorrido(casa(X,Y)), adjacente(casa(Lin,Col), casa(X,Y)), not(tem_flash(casa(X,Y))).
%Nao_InimigoD
nao_iniD(casa(Lin,Col)) :- percorrido(casa(X,Y)), adjacente(casa(Lin,Col), casa(X,Y)), not(tem_cheiroD(casa(X,Y))).

%Eh_Segura
eh_segura(casa(Lin,Col)) :- (Lin=12,Col=1); percorrido(casa(Lin,Col));
(nao_iniW(casa(Lin,Col)), nao_flash(casa(Lin,Col)), nao_poco(casa(Lin,Col)), nao_iniD(casa(Lin,Col))).

%Voltar
 voltar:- estado(casa(Lin,Col),_,_,_,_,_), Lin=12, Col=1.
 voltar:- estado(casa(Lin,Col),_,_,_,_,_), Linha is Lin-1, eh_segura(casa(Linha,Col)),Lin > 1, melhor_acao(andar_baixo), voltar.
 voltar:- estado(casa(Lin,Col),_,_,_,_,_), Coluna is Col-1, eh_segura(casa(Lin,Coluna)),Col > 1, melhor_acao(andar_esquerda), voltar.
 voltar:- estado(casa(Lin,Col),_,_,_,_,_), Coluna is Col+1, eh_segura(casa(Lin,Coluna)),Col < 12, melhor_acao(andar_direita), voltar.
 voltar:- estado(casa(Lin,Col),_,_,_,_,_), Linha is Lin+1, eh_segura(casa(Linha,Col)),Lin <12, melhor_acao(andar_cima), voltar.

 voltarg(casa(X,Y)):- estado(casa(Lin,Col),_,_,_,_,_), Lin=X, Col=Y.
 voltarg(casa(X,Y)):- estado(casa(Lin,Col),_,_,_,_,_), Linha is Lin-1, eh_segura(casa(Linha,Col)),Lin > 1, melhor_acao(andar_baixo), voltar.
 voltarg(casa(X,Y)):- estado(casa(Lin,Col),_,_,_,_,_), Coluna is Col-1, eh_segura(casa(Lin,Coluna)),Col > 1, melhor_acao(andar_esquerda), voltar.
 voltarg(casa(X,Y)):- estado(casa(Lin,Col),_,_,_,_,_), Coluna is Col+1, eh_segura(casa(Lin,Coluna)),Col < 12, melhor_acao(andar_direita), voltar.
 voltarg(casa(X,Y)):- estado(casa(Lin,Col),_,_,_,_,_), Linha is Lin+1, eh_segura(casa(Linha,Col)),Lin <12, melhor_acao(andar_cima), voltar.
 
 %voltar([X]) :- X.
%voltar([Head|Tail]):- Head, voltar(Tail).
			
eh_safenotvisit:- (eh_segura(casa(X,Y)), adjacente(casa(X,Y),casa(W,Z)), eh_segura(casa(W,Z)) , not(percorrido(casa(W,Z))),  W > 0, W< 13, Z >0, Z<13,!), voltarg(casa(W,Z)).
notvisit:- (eh_segura(casa(X,Y)), adjacente(casa(X,Y),casa(W,Z)), not(percorrido(casa(W,Z))),  W > 0, W< 13, Z >0, Z<13,!), voltarg(casa(W,Z)).

%Percepções - FIM
%/////////////////

%Decisões
%/////////////////

%Decisões Padrão
melhor_acao(pegar_ouro) :- estado(casa(Lin,Col),_,_,_,_,_) , brilho(casa(Lin,Col)), pegar_ouro.

melhor_acao(pegar_vida) :- estado(casa(Lin,Col),_,_,Vida,_,_) , Vida < 100, up(casa(Lin,Col)), pegar_vida.


%melhor_acao(arrisca) :- estado(casa(Lin,Col),Orientacao,_,_,_,_), em_loop, 
%(
%((Lin == 1, Orientacao == norte, virar_direita, virar_direita);true);
%((Lin == 12, Orientacao == sul, virar_direita, virar_direita);true);
%((Col == 1, Orientacao == oeste, virar_direita, virar_direita);true);
%((Col == 12, Orientacao == leste, virar_direita, virar_direita);true)
%),
%(
%((estado(casa(Lin,Col),Orientacao,_,_,_,_),Orientacao == norte, findall(X,(percorrido(casa(X,Col)), X < Lin ),Lista), subtractMin([1,2,3,4,5,6,7,8,9,10,11,12], Lista, List, Lin), ( length(List,0) -> (virar_direita, melhor_acao(arrisca)); ( max_member(WuLin,List), (while(WuLin,Lin);true))));true);
%((estado(casa(Lin,Col),Orientacao,_,_,_,_),Orientacao == sul, findall(X,(percorrido(casa(X,Col)), X > Lin),Lista), subtractMax([1,2,3,4,5,6,7,8,9,10,11,12], Lista, List, Lin), ( length(List,0) -> (virar_direita, melhor_acao(arrisca)); (max_member(WuLin,List),   (while(WuLin,Lin);true))));true);
%((estado(casa(Lin,Col),Orientacao,_,_,_,_),Orientacao == leste,findall(X,(percorrido(casa(Lin,X)), X > Col),Lista), subtractMax([1,2,3,4,5,6,7,8,9,10,11,12], Lista, List, Col), ( length(List,0) -> (virar_direita, melhor_acao(arrisca)); (max_member(WuCol,List),   (while(WuCol,Col);true))));true);
%((estado(casa(Lin,Col),Orientacao,_,_,_,_),Orientacao == oeste, findall(X,(percorrido(casa(Lin,X)), X < Col),Lista), subtractMin([1,2,3,4,5,6,7,8,9,10,11,12], Lista, List, Col), ( length(List,0) -> (virar_direita, melhor_acao(arrisca));(max_member(WuCol,List), (while(WuCol,Col);true))));true)
%) .

melhor_acao(arrisca) :- estado(casa(Lin,Col),Orientacao,_,_,_,_), em_loop, eh_safenotvisit.

melhor_acao(arrisca) :- estado(casa(Lin,Col),Orientacao,_,_,_,_), em_loop, notvisit.

melhor_acao(andar_direita) :- estado(casa(Lin,Col),Orientacao,_,_,_,_), Col < 12, Coluna is Col + 1, eh_segura(casa(Lin,Coluna)), not(percorrido(casa(Lin,Coluna))),
(
(Orientacao == norte, virar_direita, andar);
(Orientacao == leste, andar);
(Orientacao == sul, virar_direita, virar_direita,virar_direita, andar);
(Orientacao == oeste, virar_direita, virar_direita, andar)
) .

melhor_acao(andar_baixo) :- estado(casa(Lin,Col),Orientacao,_,_,_,_), Lin < 12, Linha is Lin + 1, eh_segura(casa(Linha,Col)), not(percorrido(casa(Linha,Col))),
(
(Orientacao == norte, virar_direita, virar_direita, andar);
(Orientacao == leste, virar_direita, andar);
(Orientacao == sul, andar);
(Orientacao == oeste, virar_direita, virar_direita, virar_direita, andar)
) .

melhor_acao(andar_esquerda) :- estado(casa(Lin,Col),Orientacao,_,_,_,_), Col > 1, Coluna is Col - 1, eh_segura(casa(Lin,Coluna)), not(percorrido(casa(Lin,Coluna))),
(
(Orientacao == norte, virar_direita, virar_direita, virar_direita, andar);
(Orientacao == leste, virar_direita, virar_direita, andar);
(Orientacao == sul, virar_direita, andar);
(Orientacao == oeste, andar)
) .

%JÁPERCORRIDO

melhor_acao(andar_cima) :- estado(casa(Lin,Col),Orientacao,_,_,_,_), Lin > 1, Linha is Lin - 1, eh_segura(casa(Linha,Col)),
(
(Orientacao == norte, andar);
(Orientacao == leste, virar_direita, virar_direita, virar_direita, andar);
(Orientacao == sul, virar_direita, virar_direita, andar);
(Orientacao == oeste, virar_direita, andar)
) .

melhor_acao(andar_direita) :- estado(casa(Lin,Col),Orientacao,_,_,_,_), Col < 12, Coluna is Col + 1, eh_segura(casa(Lin,Coluna)),
(
(Orientacao == norte, virar_direita, andar);
(Orientacao == leste, andar);
(Orientacao == sul, virar_direita, virar_direita,virar_direita, andar);
(Orientacao == oeste, virar_direita, virar_direita, andar)
) .

melhor_acao(andar_baixo) :- estado(casa(Lin,Col),Orientacao,_,_,_,_), Lin < 12, Linha is Lin + 1, eh_segura(casa(Linha,Col)),
(
(Orientacao == norte, virar_direita, virar_direita, andar);
(Orientacao == leste, virar_direita, andar);
(Orientacao == sul, andar);
(Orientacao == oeste, virar_direita, virar_direita, virar_direita, andar)
) .

melhor_acao(andar_esquerda) :- estado(casa(Lin,Col),Orientacao,_,_,_,_), Col > 1, Coluna is Col - 1, eh_segura(casa(Lin,Coluna)),
(
(Orientacao == norte,virar_direita, virar_direita, virar_direita, andar);
(Orientacao == leste, virar_direita, virar_direita, andar);
(Orientacao == sul, virar_direita, andar);
(Orientacao == oeste, andar)
) .


melhor_acao(atirar_flecha) :- estado(casa(Lin,Col),_,_,Vida,_,_), (inimigoD(casa(Lin,Col),_);inimigoW(casa(Lin,Col),_)),
Vida >= 50, atirar_flecha.

run :- melhor_acao(X), X, estado(_,_,_,_,Ouros,_), (Ouros=3;run).

while(Wu, X) :- (Wu \= X), andar, (Wu < X -> (X is X - 1);(X is X + 1)), while(Wu, X).

subtractMin(A,B,C,L):- findall(X, (member(X,A), not(member(X,B)), X < L), C).
subtractMax(A,B,C,L):- findall(X, (member(X,A), not(member(X,B)), X > L), C).
em_loop:- acoes(X),[C|[D|[E|[R|[T|[H|_]]]]]] = X, tamanholista(X,N), N>5, C==E,E==T,D ==R,R==H.