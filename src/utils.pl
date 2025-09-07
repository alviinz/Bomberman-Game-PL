:- module(utils, [createPoint/3]).

/*
Cria um ponto que tem a coordenada X e Y.

@predicate createPoint(?X, ?Y, ?Point).

@param X a coordenada X do ponto.
@param Y a coordenada Y do ponto.
@param Point o ponto com as coordenas X e Y.

@example
    createPoint(10, 8, Point).
    Point = 10-8.

    createPoint(X, Y, 10-8).
    X = 10,
    Y = 8.

    createPoint(X, _, 10-8).
    X = 10.
*/
createPoint(X, Y, Point) :-
    Point = X-Y.


wall_symbol('#').
player_symbol('@').
