:- use_module(gameLoop).
:- initialization(main).

main :-
    Configs = configs{height: 9, width: 19, timer: 120},
    init_game(Configs),
    halt.

