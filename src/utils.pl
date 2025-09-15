:- module(utils, [createPoint/3,
                  neighbors/2,
                  box_survives/2,
                  is_dead/1,
		              is_not_a_wall/2
                  ]).
:- use_module(bombs).


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
Filtra os pontos de uma explosão para remover os que colidem com paredes.

@predicate is_not_a_wall(+Walls, +Point).
@param Walls a lista de pontos das paredes.
@param Point o ponto a ser verificado.
@return `true` se o ponto não for membro da lista de paredes.
*/
is_not_a_wall(Walls, Point) :-
    \+ member(Point, Walls).

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