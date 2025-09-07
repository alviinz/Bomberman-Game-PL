:- module(board, [createBoard/2]).
:- use_module(utils).

/*
Cria um tabuleiro (Board) a partir de um conjunto de Configurações.

@predicate createBoard(+Configs, -Board).

@param Configs uma dict contendo as configurações do jogo.

@return Um tabuleiro (Board) no formato de dict que contém as informações do tabuleiro.
*/
createBoard(Configs, Board) :-
    createWalls(Configs, Walls),
    createPoint(2 , 2, Player),
    Board = board{walls: Walls, player: Player}. 

/*
Cria uma lista contendo todas as coordenadas das paredes indestrutíveis do jogo.

@predicate creatWalls(+Configs, -Walls).

@param Configs uma dict contendo as configurações do jogo.

@return uma lista contendo todas as coordenas das paredes.
*/
createWalls(Configs, Walls) :-
    findall(Point, 
            (between(1, Configs.width, X),
             member(Y, [1, Configs.height]),
             createPoint(X, Y, Point)), 
            Walls1),
    findall(Point, 
            (member(X, [1, Configs.width]),
             between(2, Configs.height, Y),
             createPoint(X, Y, Point)), 
            Walls2),
    findall(Point,
            (MaxX is Configs.width - 2,
             MaxY is Configs.height - 2,
             between(3, MaxX, X),
             between(3, MaxY, Y),
             X mod 2 =\= 0,
             Y mod 2 =\= 0,
             createPoint(X, Y, Point)),
            Walls3),
    append([Walls1, Walls2, Walls3], Walls).

