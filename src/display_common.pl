:- module(display_common, [exitDisplay/1,
                            display_game_win/1,
                            display_game_over/1]).
:- use_module(ansi_terminal).

%%OLHAR ESSA BOSTA AQUI, POIS FOI UTILIZADA PRA TELA DE VENCER E PERDER

exitDisplay(Configs) :-
   clearDisplay,
    get_dict(width, Configs, W),
    get_dict(height, Configs, H),
    MsgX is W // 2 - 5,
    MsgY is H // 2,
    writeAt(MsgX, MsgY, "GOOD BYE! SEE YOU LATER...", '97'),
    movePointer(0, H + 2).

/*
Mostra a tela de Game Win.

@predicate display_game_win(+Configs).
*/
display_game_win(Configs) :-
    clearDisplay,
    get_dict(width, Configs, W),
    get_dict(height, Configs, H),
    MsgX is W // 2 - 5,
    MsgY is H // 2,
    writeAt(MsgX, MsgY, "YOU WIN!", '92'),
    movePointer(0, H + 2).

/*
Mostra a tela de Game Over.

@predicate display_game_over(+Configs).
*/
display_game_over(Configs) :-
    clearDisplay,
    get_dict(width, Configs, W),
    get_dict(height, Configs, H),
    MsgX is W // 2 - 5,
    MsgY is H // 2,
    writeAt(MsgX, MsgY, "GAME OVER!", '91'),
    movePointer(0, H + 2).
