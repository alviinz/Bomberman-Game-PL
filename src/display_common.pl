:- module(display_common, [exitDisplay/1]).
:- use_module(ansi_terminal).

exitDisplay(Board) :-
   clearDisplay,
    (Board.game_over ->
        writeAt(1, 1, 'Parabens! Voce venceu o jogo! :)')
    ;
    writeAt(1, 1, 'Até logo. Bye, bye!')),
    movePointer(0, 3),
    !.


/*
Mostra o ecrã de Game Over.

@predicate display_game_over(+Configs).
*/
display_game_over(Configs) :-
    % Pode personalizar as coordenadas e a mensagem como quiser
    get_dict(width, Configs, W),
    get_dict(height, Configs, H),
    MsgX is W // 2 - 5,
    MsgY is H // 2,
    writeAt(MsgX, MsgY, "GAME OVER!", [red]),
    movePointer(0, H + 2). % Move o cursor para baixo do tabuleiro