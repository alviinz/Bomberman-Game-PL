:- module(ansi_terminal, [clearDisplay/0,
                          movePointer/2,
                          writeAt/3,
                          hideCursor/0,
                          showCursor/0]).

/*
Limpa o teminal

@predicade clearDisplay 
*/
clearDisplay :-
    format('\033[2J').

/*
Move o cursor para uma posição específica do terminal.

@predicade movePointer(+X, +Y)

@param X é a posição da coluna.
@param Y é a posição da linha.
*/
movePointer(X, Y) :-
    format('\033[~d;~dH', [Y, X]).

/*
Move o cursor e escreve em uma posição específica do terminal.

@predicade movePointer(+X, +Y, +Text)

@param X    é a posição da coluna.
@param Y    é a posição da linha.
@param Text é a string ou charactere que deseja escrever no terminal.
*/
writeAt(X, Y, Text) :-
    movePointer(X, Y),
    format('~a', [Text]).

/*
Esconde o cursor do terminal.

@predicate hideCursor.
*/
hideCursor :-
    format('\033[?25l').

/*
Mostra o cursor do terminal.

@predicate showCursor.
*/
showCursor :-
    format('\033[?25h').
