:- module(player, [movePlayer/3]).
:- use_module(utils).

/*
Move o persongem. Em outras palavras, cria uma nova posição pro personagem a partir da sua posição anterior e de um caractere.

@predicate movePlayer(+Char, +Player, -NewPlayer).

@param Char  é o charactere que representará para que direção as coordenadas do personagens serão atualizadas.
@param Player é a coordenada, posição, atual do personagem.

@return as novas coordenas do personagem se houver fatos para o charactere recebido, caso não haja fatos sobre o charactere será repedito a posição atual do personagem.
*/
movePlayer(Char, Player, NewPlayer) :-
    move(Char, Dx, Dy) ->
        (createPoint(X, Y, Player),
         NewX is Dx + X,
         NewY is Dy + Y,
         createPoint(NewX, NewY, NewPlayer));
        NewPlayer = Player.

/*
@predicate move(+Char, -X, -Y).

@param Char o caractere que representa a direção que as coordenadas devem ser atualiazadas.

@return X o incremento, ou decremento, que a coordenada X deve receber.
@return Y o incremento, ou decremento, que a coordenada Y deve receber.
*/
move('w',  0, -1).
move('s',  0,  1).
move('a', -1,  0).
move('d',  1,  0).
move('A',  0, -1).
move('B',  0,  1).
move('D', -1,  0).
move('C',  1,  0).
