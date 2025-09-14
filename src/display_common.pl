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
