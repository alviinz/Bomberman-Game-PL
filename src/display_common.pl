:- module(display_common, [exitDisplay/1]).
:- use_module(ansi_terminal).

exitDisplay(Configs) :-
    clearDisplay,
    writeAt(0, 1, 'bye, bye. See you later!'),
    movePointer(0, 3),
    !.
