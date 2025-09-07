:- use_module(display_board).
:- use_module(ansi_terminal).
:- use_module(board).
:- initialization(main).

main :-
    Configs = configs{height: 9, width: 19, timer: 120},
    init_game(Configs),
    movePointer(0, 11),
    halt.

/*
inicia um jogo com um conjunto de configurações (Configs).

@predicate init_game(+Configs).

@param Configs uma dict contendo as configurações do jogo.
*/
init_game(Configs) :-
    createBoard(Configs, Board),
    displayBoard(Board).

