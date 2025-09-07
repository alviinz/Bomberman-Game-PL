:- module(board, [createBoard/2,
                  updateBoard/3]).
                  
:- use_module(utils).

/*
Cria um tabuleiro (Board) a partir de um conjunto de Configurações.

@predicate createBoard(+Configs, -Board).

@param Configs uma dict contendo as configurações do jogo.

@return Um tabuleiro (Board) no formato de dict que contém as informações do tabuleiro.
*/
createBoard(Configs, Board) :-
    createWalls(Configs, Walls),
    createBoxes(Configs, Walls, Boxes),
    createPoint(2 , 2, Player),
    Board = board{walls: Walls, boxes: Boxes, player: Player}. 

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

/*
Cria uma lista contendo todas as coordenadas das caixas destrutíveis do jogo.

@predicate createBoxes(+Configs, +Walls, -Boxes).

@param Configs uma dict contendo as configurações do jogo.

@param Walls  uma lista contendo as coordenadas das paredes indestrutíveis do jogo.

@return uma lista contendo as coordenadas das caixas destrutíveis do jogo.
 */
createBoxes(Configs, Walls, Boxes) :-
    InitialPlayerPos = 2-2,
    neighbors(InitialPlayerPos, InvalidPositions),
    findall(Point,
            (MaxX is Configs.width - 1,
             MaxY is Configs.height - 1,
             between(2, MaxX, X),
             between(2, MaxY, Y),
             createPoint(X, Y, Point),
             \+ member(Point, Walls),
             \+ member(Point, InvalidPositions),
             random(0.0, 1.0, R),
             R < 0.7),
            Boxes).

/*
Atualiza um tabuleiro a partir de uma nova posição do jogador.

@predicate updateBoard(+Board, +NewPlayer, -NewBoard). 

@param Board     uma dict contendo as informações do Tabuleiro.
@param NewPlayer as coordenadas da nova posição do jogador.

@return uma nova dict contendo as informações do tabuleiro atualizada.
*/
updateBoard(Board, NewPlayer, NewBoard) :-
    (member(NewPlayer, Board.walls);
    member(NewPlayer, Board.boxes)) ->
        NewBoard = Board;
        NewBoard = Board.put(player, NewPlayer).