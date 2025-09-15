:- module(display_common, [exitDisplay/1,
                            display_game_win/1,
                            display_game_over/1]).
:- use_module(ansi_terminal).
:- use_module(gameLoop). 


exitDisplay(Configs) :-
   clearDisplay,
    get_dict(width, Configs, W),
    get_dict(height, Configs, H),
    MsgX is W // 2 - 5,
    MsgY is H // 2,
    writeAt(MsgX, MsgY, "TCHAU, ATÉ A PRÓXIMA!", '1;97'),
    flush_output,
    sleep(1),
    writeAt(MsgX, MsgY+1, "ESPERO QUE VOCÊ VOLTE...", '1;97'),
    flush_output,
    sleep(1),
    movePointer(0, H + 2),
    showCursor.

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
    writeAt(MsgX, MsgY, "ESPERA UM POUCO", '1;92'),
    flush_output,
    sleep(0.6),
    writeAt(MsgX+15, MsgY, ".", '1;92'),
    flush_output,
    sleep(0.6),
    writeAt(MsgX+16, MsgY, ".", '1;92'),
    flush_output,
    sleep(0.6),
    writeAt(MsgX+17, MsgY, ".", '1;92'),
    flush_output,
    sleep(1),
    writeAt(MsgX, MsgY+1, "VOCÊ VENCEU?", '1;92'),
    flush_output,
    sleep(0.6),
    writeAt(MsgX+13, MsgY+1, ":OO", '1;92'),
    flush_output,
    sleep(1),
    writeAt(MsgX, MsgY+2, "PARABÉNS!", '1;92'),
    flush_output,
    sleep(0.6),
    writeAt(MsgX+10, MsgY+2, "FICO FELIZ!", '1;92'),
    flush_output,
    sleep(1),
    ask_replay_loop(Configs).

/*
Mostra a tela de Game Over e inicia o loop para perguntar se quer jogar novamente.

@predicate display_game_over(+Configs).
*/
display_game_over(Configs) :-
    clearDisplay,
    get_dict(width, Configs, W),
    get_dict(height, Configs, H),
    MsgX is W // 2 - 5,
    MsgY is H // 2,
    writeAt(MsgX, MsgY, "POXA", '1;91'),
    flush_output,
    sleep(0.6),
    writeAt(MsgX+4, MsgY, ".", '1;91'),
    flush_output,
    sleep(0.6),
    writeAt(MsgX+5, MsgY, ".", '1;91'),
    flush_output,
    sleep(0.6),
    writeAt(MsgX+6, MsgY, ".", '1;91'),
    flush_output,
    sleep(1),
    writeAt(MsgX, MsgY+1, "QUE PENA", '1;91'),
    flush_output,
    sleep(0.6),
    writeAt(MsgX+9, MsgY+1, "VOCÊ PERDEU.", '1;91'),
    flush_output,
    sleep(1),
    writeAt(MsgX, MsgY, "HA", '1;97'),
    flush_output,
    sleep(0.6),
    writeAt(MsgX+2, MsgY+2, "HA", '1;97'),
    flush_output,
    sleep(0.6),
    writeAt(MsgX, MsgY+4, "HA", '1;97'),
    flush_output,
    sleep(0.6),
    writeAt(MsgX+2, MsgY+6, "HA", '1;97'),
    flush_output,
    sleep(1),
    ask_replay_loop(Configs).

/*
Mostra o prompt "Jogar Novamente?" e espera pela decisão do utilizador.
*/
ask_replay_loop(Configs) :-
    get_dict(width, Configs, W),
    get_dict(height, Configs, H),
    OptionX is W // 2 - 12,
    OptionY is H // 2 + 2,
    clearDisplay,
    writeAt(OptionX, OptionY, "Jogar Novamente? [1] SIM | [2] NÃO", '1;37'), 
    flush_output,
    
    get_single_char(Code),
    char_code(Char, Code),
    upcase_atom(Char, Choice), 
    handle_replay_choice(Choice, Configs).

/*
Trata da escolha do utilizador (1 ou 2).
*/
handle_replay_choice('1', Configs) :-
    init_game(Configs). 

handle_replay_choice('2', Configs) :-
    exitDisplay(Configs), 
    showCursor,
    halt.

handle_replay_choice(_, Configs) :-
    get_dict(width, Configs, W),
    get_dict(height, Configs, H),
    writeAt(W // 2 - 7, H // 2 + 4, "Opção inválida!", '1;91'),
    sleep(1.5),
    
    writeAt(W // 2 - 7, H // 2 + 4, "                 "),
    
    ask_replay_loop(Configs).
