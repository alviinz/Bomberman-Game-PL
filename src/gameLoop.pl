:- module(gameLoop, [init_game/1]).
:- use_module(board).
:- use_module(player).
:- use_module(display_board).
:- use_module(display_common).
:- use_module(ansi_terminal).
:- use_module(utils).

/*
inicia um jogo com um conjunto de configurações (Configs).

@predicate init_game(+Configs).

@param Configs uma dict contendo as configurações do jogo.
*/
init_game(Configs) :-
    hideCursor,
    createBoard(Configs, Board),
    gameLoop(Configs, Board),
    showCursor.

/*
O loop de uma partida. Representa, portanto, a execução de uma única partida do jogo.

@predicate gameLoop(+Configs, +Board).

@param Configs uma dict representando as configurações do jogo.
@param Board   uma dict representando o tabuleiro.

*/
gameLoop(Configs, Board) :-
    displayBoard(Board),
    get_single_char(Code),
    char_code(Char, Code),
    (Char = ' ' ->
        placeBombs(Board, AlmostBoard);
        movePlayer(Char, Board.player, NewPlayer),
        updateBoard(Board, NewPlayer, AlmostBoard)
    ),
    update_bombs_and_create_explosions(AlmostBoard, BoardWithNewExplosions),
    update_existing_explosions(BoardWithNewExplosions, FinalBoard),
    write(FinalBoard.explosions),
    (Char = 'q' -> exitDisplay(Configs) ; gameLoop(Configs, FinalBoard)).
