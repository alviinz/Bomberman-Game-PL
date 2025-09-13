:- module(utils, [createPoint/3, 
                  neighbors/2, 
                  get_bomb_position/2, 
                  decrementTimerBomb/2, 
                  is_bomb_active/1,
                  explode_bombs/2,
                  createExplosion/2,
		  update_bombs_and_create_explosions/2,
		  update_existing_explosions/2,
		  bomb_is_ready/1,
		  get_explosion_position/2,
		  is_explosion_active/1]).
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

/* Predicado que dada uma lista de explosões define se uma caixa está ativa ou não.
 * @predicate box_survives(+NewExplosions, -Box)
 * return se uma caixa está ativa ou não.
 */
box_survives(NewExplosions, Box) :-
    \+ (member(Explosion, NewExplosions), member(Box, Explosion.points)).


/*Um predicado simples para retornar a posição de uma bomba.
 * @predicate get_bomb_position(+Bomb, - Position)
 * return Position, a posição de uma bomba.
 */
get_bomb_position(Bomb, Position) :-
    Position = Bomb.position.

/*Um predicado simples para retornar a posição de uma explosão.
 * @predicate get_explosion_position(+Explosion, - Position)
 * return Position, a posição de uma explosão.
 */
get_explosion_position(Explosion,Position):-
	Position = Explosion.points.

/*Um predicado simples para decrementar o tempo de uma bomba
 * @predicate decrementTimerBomb(+Bomb, - NewBomb)
 * return NewBomb, uma bomba com o timer atualizado.
 */
decrementTimerBomb(Bomb,NewBomb):-
	Y is Bomb.timer - 1,
	NewBomb = Bomb.put(timer,Y).

/*Um predicado simples para decrementar o tempo de uma explosão
 * @predicate decrementTimerExplosion(+Explosion, - NewExplosion)
 * return NewExplosion, uma explosão com o timer atualizado.
 */
decrementTimerExplosion(Explosion, NewExplosion) :-
    T is Explosion.timer - 1,
    NewExplosion = Explosion.put(timer, T).


update_bombs_and_create_explosions(OldBoard, NewBoard) :-
    AllBombs = OldBoard.bombs,
    maplist(decrementTimerBomb, AllBombs, BombsDecremented),
    BoardWithDecrementedBombs = OldBoard.put(bombs, BombsDecremented),
    explode_bombs(BoardWithDecrementedBombs, NewBoard).

update_existing_explosions(OldBoard, NewBoard) :-
    OldExplosions = OldBoard.explosions,
    maplist(decrementTimerExplosion, OldExplosions, DecrementedExplosions),
    include(is_explosion_active, DecrementedExplosions, ActiveExplosions),
    NewBoard = OldBoard.put(explosions, ActiveExplosions).

/* Um predicado que nos informa se uma bomba está ativa ou não.
 * @predicate is_bomb_active(+Bomb)
 * return booleano que dirá se o tempo da bomba é maior que zero ou não.
 */
is_bomb_active(Bomb) :-
    Bomb.timer > 0.
bomb_is_ready(Bomb) :-
    Bomb.timer =:= 0.

/*Um predicado responsável por explodir as bombas
 *@predicate explode_bomb(+OldBoard, -NewBoard)
 *return um novo mapa com explosões
 */
explode_bombs(OldBoard, NewBoard) :-
    AllBombs = OldBoard.bombs,
    include(bomb_is_ready, AllBombs, BombsToExplode),
    ( BombsToExplode == [] ->
        NewBoard = OldBoard
    ;
        maplist(createExplosion, BombsToExplode, NewExplosions),
        AllBoxes = OldBoard.boxes,
        include(box_survives(NewExplosions), AllBoxes, RemainingBoxes),
        include(is_bomb_active, AllBombs, RemainingBombs),
        OldExplosions = OldBoard.explosions,
        append(OldExplosions, NewExplosions, AllExplosions),
        BoardWithUpdatedBombs = OldBoard.put(bombs, RemainingBombs),
        BoardWithUpdatedBoxes = BoardWithUpdatedBombs.put(boxes, RemainingBoxes),
        NewBoard = BoardWithUpdatedBoxes.put(explosions, AllExplosions)
    ).
 

/*Um predicado responsável por criar explosões
 *@predicate createExplosion(+Bomb, -Explosion)
 *return um dicionário representando uma explosão
 */
createExplosion(Bomb, Explosion) :-
	Bomb.timer =:= 0,
	neighbors(Bomb.position, Points),
	Explosion = explosion{points: Points, timer: 2}.

is_explosion_active(Explosion) :-
	Explosion.timer > 0.
