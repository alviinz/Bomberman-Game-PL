criar_linha(0, _, []).
criar_linha(NumColunas, Elemento, [Elemento | RestoDaLinha]) :-
    NumColunas > 0,
    C1 is NumColunas - 1,
    criar_linha(C1, Elemento, RestoDaLinha).

criar_matriz(0, _, _, []).
criar_matriz(NumLinhas, NumColunas, Elemento, [Linha | RestoDaMatriz]) :-
    NumLinhas > 0,
    criar_linha(NumColunas, Elemento, Linha),
    L1 is NumLinhas - 1,
    criar_matriz(L1, NumColunas, Elemento, RestoDaMatriz).

imprimir_mapa_simples([]).
imprimir_mapa_simples([Linha | RestoDoMapa]) :-
    imprimir_linha_simples(Linha),
    nl,
    imprimir_mapa_simples(RestoDoMapa).

imprimir_linha_simples([]).
imprimir_linha_simples([Elemento | RestoDaLinha]) :-
    write(Elemento),
    imprimir_linha_simples(RestoDaLinha).


criar_linha_cheia(0, _, []).
criar_linha_cheia(NumColunas, Elemento, [Elemento | RestoDaLinha]) :-
    NumColunas > 0,
    C1 is NumColunas - 1,
    criar_linha_cheia(C1, Elemento, RestoDaLinha).

criar_linha_meio(Largura, Borda, Fundo, [Borda | ConteudoComBordaFinal]) :-
    Largura > 1,
    LarguraDoMiolo is Largura - 2,
    criar_linha_cheia(LarguraDoMiolo, Fundo, Miolo),
    append(Miolo, [Borda], ConteudoComBordaFinal).
criar_linha_meio(1, Borda, _, [Borda]). % Caso especial: linha com apenas 1 de largura.

gerar_miolo(0, _, []).
gerar_miolo(N, LinhaDoMeio, [LinhaDoMeio | RestoDoMiolo]) :-
    N > 0,
    N1 is N - 1,
    gerar_miolo(N1, LinhaDoMeio, RestoDoMiolo).

criar_mapa_com_borda(Largura, Altura, BordaChar, FundoChar, MapaFinal) :-
    Altura > 1, Largura > 0, % Condições para ter um miolo
    % 1. Cria a linha de cima (e de baixo, que é igual)
    criar_linha_cheia(Largura, BordaChar, LinhaDeBorda),
    
    % 2. Cria uma linha modelo para o miolo
    criar_linha_meio(Largura, BordaChar, FundoChar, LinhaDeMiolo),
    
    % 3. Gera todas as linhas do miolo
    NumLinhasMiolo is Altura - 2,
    gerar_miolo(NumLinhasMiolo, LinhaDeMiolo, Miolo),
    
    % 4. Junta tudo: [Linha de Cima | ...Miolo... | Linha de Baixo]
    append([LinhaDeBorda | Miolo], [LinhaDeBorda], MapaFinal).

criar_mapa_com_borda(Largura, 1, BordaChar, _, [LinhaDeBorda]) :-
    criar_linha_cheia(Largura, BordaChar, LinhaDeBorda).
criar_mapa_com_borda(Largura, 0, _, _, []).

