:- module(display_board, [displayPoint/2,
                          displayPoints/2,
                          displayBoard/1]).

:- use_module(ansi_terminal).
:- use_module(utils).

/*
escreve em uma coordenada da tela (Point) um símbolo (Symboll).

@predicate displayPoint(+Point, +Symbol).

@param Point  o ponto que contém as coordenadas X e Y para escrever no terminal.
@param Symbol o character que deve ser impresso na tela.
*/
displayPoint(Point, Symbol) :-
    createPoint(X, Y, Point),
    writeAt(X, Y, Symbol).

/*
escreve um símbolo (Symboll) em todas as coordenadas contidas em uma lista de pontos (Points).

@predicate displayPoints(+Points, +Symbol).

@param Points uma lista de pontos que contêm as coordenadas X's e Y's para escrever no terminal.
@param Symbol o character que deve ser impresso na tela.
*/
displayPoints([], _).
displayPoints(Points, Symbol) :-
    [P|Ps] = Points,
    displayPoint(P, Symbol),
    displayPoints(Ps, Symbol).

/*
escreve o tabuleiro no terminal.

@predicate displayBoard(+Board).

@param Board o tabuleiro que deve ser impresso no terminal.
*/
displayBoard(Board) :-
    clearDisplay,
    displayPoints(Board.walls, '#'),
    displayPoints(Board.boxes, '='),
    displayPoint(Board.player, '@').

