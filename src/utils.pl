:- module(utils, [createPoint/3,
                  neighbors/2,
                  get_bomb_position/2,
                  get_explosion_position/2,
                  box_survives/2,
                  update_bombs_and_create_explosions/2,
                  update_existing_explosions/2,
                  explode_bombs/2,
                  createExplosion/3,
                  decrementTimerExplosion/2,
                  is_explosion_active/1,
                  check_player_death/2,
		  is_dead/1
                  ]).


/*
Cria um ponto a partir das coordenadas X e Y.

@predicate createPoint(?X, ?Y, ?Point).
@param X a coordenada X do ponto.
@param Y a coordenada Y do ponto.
@return Point o termo correspondente ao ponto (ex: `X-Y`).
*/
createPoint(X, Y, Point) :-
    Point = X-Y.

/*
Calcula os pontos vizinhos (adjacentes e o próprio ponto) de um ponto dado.

@predicate neighbors(+Point, -Neighbors).
@param Point o ponto central.
@return Neighbors uma lista com os 5 pontos (centro, acima, abaixo, esquerda, direita).
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

/*
Verifica se uma caixa sobrevive a um conjunto de explosões.

@predicate box_survives(+NewExplosions, +Box).
@param NewExplosions uma lista de `dicts` de explosão.
@param Box o ponto da caixa a ser verificado.
@return `true` se a caixa não estiver em nenhum ponto de nenhuma explosão.
*/
box_survives(NewExplosions, Box) :-
    \+ (member(Explosion, NewExplosions), member(Box, Explosion.points)).

/*
Verifica se uma bomba está pronta para explodir com base no tempo.

@predicate is_bomb_ready(+CurrentTime, +Bomb).
@param CurrentTime o tempo atual do sistema.
@param Bomb o `dict` da bomba, que deve conter a chave `plant_time`.
@return `true` se o tempo decorrido for maior que o tempo de vida da bomba.
*/
is_bomb_ready(CurrentTime, Bomb) :-
    ElapsedTime is CurrentTime - Bomb.plant_time,
    BombLifetime is 1.5, 
    ElapsedTime > BombLifetime.

/*
Verifica se uma explosão ainda está ativa (com base no seu timer).

@predicate is_explosion_active(+Explosion).
@param Explosion o `dict` da explosão.
@return `true` se o `timer` da explosão for maior que 0.
*/
is_explosion_active(Explosion) :-
    Explosion.timer > 0.

/*
Filtra os pontos de uma explosão para remover os que colidem com paredes.

@predicate is_not_a_wall(+Walls, +Point).
@param Walls a lista de pontos das paredes.
@param Point o ponto a ser verificado.
@return `true` se o ponto não for membro da lista de paredes.
*/
is_not_a_wall(Walls, Point) :-
    \+ member(Point, Walls).

/*
Atualiza o estado de todas as bombas, criando explosões para as que estão prontas.

@predicate update_bombs_and_create_explosions(+OldBoard, -NewBoard).
@param OldBoard o estado do tabuleiro antes da atualização.
@return NewBoard o estado do tabuleiro após a atualização das bombas.
*/
update_bombs_and_create_explosions(OldBoard, NewBoard) :-
    get_time(CurrentTime),
    AllBombs = OldBoard.bombs,
    partition(is_bomb_ready(CurrentTime), AllBombs, BombsToExplode, ActiveBombs),
    (   BombsToExplode == [] ->
        NewBoard = OldBoard
    ;
        explode_bombs(OldBoard, BombsToExplode, ActiveBombs, NewBoard)
    ).

/*
Processa a explosão de um conjunto de bombas.

@predicate explode_bombs(+OldBoard, +BombsToExplode, +RemainingBombs, -NewBoard).
@param OldBoard o estado do tabuleiro.
@param BombsToExplode a lista de bombas que devem explodir agora.
@param RemainingBombs a lista de bombas que continuarão no tabuleiro.
@return NewBoard o novo estado do tabuleiro com as explosões e caixas destruídas.
*/
explode_bombs(OldBoard, BombsToExplode, RemainingBombs, NewBoard) :-
    maplist(createExplosion(OldBoard), BombsToExplode, NewExplosions),
    AllBoxes = OldBoard.boxes,
    include(box_survives(NewExplosions), AllBoxes, RemainingBoxes),
    OldExplosions = OldBoard.explosions,
    append(OldExplosions, NewExplosions, AllExplosions),
    BoardWithUpdatedBombs = OldBoard.put(bombs, RemainingBombs),
    BoardWithUpdatedBoxes = BoardWithUpdatedBombs.put(boxes, RemainingBoxes),
    NewBoard = BoardWithUpdatedBoxes.put(explosions, AllExplosions).

/*
Cria um `dict` de explosão a partir de uma bomba, respeitando as paredes.

@predicate createExplosion(+Board, +Bomb, -Explosion).
@param Board o estado do tabuleiro, para verificar as paredes.
@param Bomb a bomba que está a explodir.
@return Explosion o `dict` da nova explosão.
*/
createExplosion(Board, Bomb, Explosion) :-
    neighbors(Bomb.position, PotentialPoints),
    Walls = Board.walls,
    include(is_not_a_wall(Walls), PotentialPoints, ActualPoints),
    Explosion = explosion{points: ActualPoints, timer: 3}.

/*
Atualiza o estado das explosões existentes, diminuindo o seu timer.

@predicate update_existing_explosions(+OldBoard, -NewBoard).
@param OldBoard o estado do tabuleiro antes da atualização.
@return NewBoard o estado do tabuleiro com as explosões atualizadas.
*/
update_existing_explosions(OldBoard, NewBoard) :-
    OldExplosions = OldBoard.explosions,
    maplist(decrementTimerExplosion, OldExplosions, DecrementedExplosions),
    include(is_explosion_active, DecrementedExplosions, ActiveExplosions),
    NewBoard = OldBoard.put(explosions, ActiveExplosions).

/*
Cria um novo `dict` de explosão com o timer decrementado.

@predicate decrementTimerExplosion(+Explosion, -NewExplosion).
@param Explosion a explosão original.
@return NewExplosion a explosão com o timer atualizado.
*/
decrementTimerExplosion(Explosion, NewExplosion) :-
    T is Explosion.timer - 1,
    NewExplosion = Explosion.put(timer, T).


/*
Obtém a posição de uma bomba.

@predicate get_bomb_position(+Bomb, -Position).
@param Bomb o `dict` da bomba.
@return Position a posição da bomba.
*/
get_bomb_position(Bomb, Position) :-
    Position = Bomb.position.

/*
Obtém os pontos de uma explosão.

@predicate get_explosion_position(+Explosion, -Points).
@param Explosion o `dict` da explosão.
@return Points a lista de pontos da explosão.
*/
get_explosion_position(Explosion, Points) :-
    Points = Explosion.points.

/*
 *Unifica caso o player esteja morto, e não caso o player esteja vivo.
 *@predicate is_dead(+Board).
 *@param Board o mapa com as posições do player e das explosões.
 *@return Verdadeiro caso o player esteja morto, falso caso não esteja.
 */ 
is_dead(Board) :-
    PlayerPos = Board.player,
    Explosions = Board.explosions,
    maplist(get_explosion_position, Explosions, ListOfExplosionPointsLists),
    flatten(ListOfExplosionPointsLists, AllExplosionPoints),
    member(PlayerPos, AllExplosionPoints).


