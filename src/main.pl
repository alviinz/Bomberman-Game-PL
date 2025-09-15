:- use_module(menu). 
:- initialization(main).

main :-
    Configs = configs{height: 9, width: 19},
    run_menu(Configs), 
    halt.
