:- module(utils, [createPoint/3, 
                  neighbors/2, 
                  get_bomb_position/2, 
                  decrementTimerBomb/2, 
                  is_bomb_active/1]).

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

/*
Dado um ponto (Point), retorna uma lista com os pontos vizinhos (Neighbors).

@predicate neighbors(+Point, -Neighbors).

@param Point     o ponto que contém as coordenadas X e Y.

@return Neighbors uma lista com os pontos vizinhos (incluindo o próprio ponto).
*/
neighbors(X-Y, Neighbors) :-
    X1 is X - 1, X2 is X + 1,
    Y1 is Y - 1, Y2 is Y + 1,
    createPoint(X, Y, P0), 
    createPoint(X1, Y, P1),
    createPoint(X2, Y, P2),
    createPoint(X, Y1, P3),
    createPoint(X, Y2, P4),
    Neighbors = [P0, P1, P2, P3, P4].

/*Um predicado simples para retornar a posição de uma bomba.
 * @predicate get_bomb_position(+Bomb, - Position)
 * return Position, a posição de uma bomba.
 */
get_bomb_position(Bomb, Position) :-
    Position = Bomb.position.

decrementTimerBomb(Bomb,NewBomb):-
	Y is Bomb.timer - 1,
	NewBomb = Bomb.put(timer,Y).

is_bomb_active(Bomb) :-
    Bomb.timer > 0.
