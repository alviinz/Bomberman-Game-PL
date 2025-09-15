:- module(display_board, [displayPoint/2,
                          displayPoints/2,
                          displayBoard/1]).

:- use_module(ansi_terminal).
:- use_module(utils).
:- use_module(bombs). 

/*
 * escreve em uma coordenada da tela (Point) um símbolo (Symboll).
 * 
 * @predicate displayPoint(+Point, +Symbol).
 * 
 * @param Point  o ponto que contém as coordenadas X e Y para escrever no terminal.
 * @param Symbol o character que deve ser impresso na tela.
*/
displayPoint(Point, Symbol) :-
    createPoint(X, Y, Point),
    writeAt(X, Y, Symbol).

/*
 * faz a mesma coisa que o displayPoint/2, com a adição de personalizar o símbolo.
 * 
 * @predicate displayPoint(+Point, +Symbol, +Codes).
 * 
 * @param Point  o ponto que contém as coordenadas X e Y para escrever no terminal.
 * @param Symbol o character que deve ser impresso na tela.
 * @param Codes  os códigos para a personalização do caractere no formato ANSI. (veja a documentação de persnonalizeTerm para a consulta dos códigos disponíveis)
*/
displayPoint(Point, Symbol, Codes) :-
    createPoint(X, Y, Point),
    writeAt(X, Y, Symbol, Codes).

/*
 * escreve um símbolo (Symboll) em todas as coordenadas contidas em uma lista de pontos (Points).
 * 
 * @predicate displayPoints(+Points, +Symbol).
 * 
 * @param Points uma lista de pontos que contêm as coordenadas X's e Y's para escrever no terminal.
 * @param Symbol o character que deve ser impresso na tela.
*/
displayPoints([], _).
displayPoints(Points, Symbol) :-
    [P|Ps] = Points,
    displayPoint(P, Symbol),
    displayPoints(Ps, Symbol).

/*
 * possue o mesmo funcionamento do displayPoints/2, mas com a adição de personalização do símbolo.
 * 
 * @predicate displayPoints(+Points, +Symbol, +Codes).
 * 
 * @param Points uma lista de pontos que contêm as coordenadas X's e Y's para escrever no terminal.
 * @param Symbol o character que deve ser impresso na tela.
 * @param Codes  os códicos de personalização do caractere no formato ANSI. (veja a documentação de personalizeTerm para a consulta dos códigos de cores disponíveis).
*/
displayPoints([], _, _).
displayPoints(Points, Symbol, Color) :-
    [P|Ps] = Points,
    displayPoint(P, Symbol, Color),
    displayPoints(Ps, Symbol, Color).

/*
 * escreve o tabuleiro no terminal.
 * 
 * @predicate displayBoard(+Board).
 * 
 * @param Board o tabuleiro que deve ser impresso no terminal.
*/
displayBoard(Board) :-
    clearDisplay,
    wall_symbol(Ws), player_symbol(Ps), bomb_symbol(Bs), box_symbol(Boxs), explosion_symbol(Es),door_symbol(Ds), key_symbol(Ks), 
    getBombsPoints(Board.bombs, BombsPoints),
    getExplosionsPoints(Board.explosions, ExplosionPoints),
    displayPoints(Board.walls, Ws), 
    displayPoints(Board.boxes, Boxs, '30;103'), 
    displayPoints(BombsPoints, Bs, '91;5'),
    displayPoints(ExplosionPoints, Es),
    displayPoint(Board.player, Ps),
    (Board.has_key -> true ; \+ member(Board.key_position, Board.boxes)
    ->  displayPoint(Board.key_position, Ks, '33') ; true),
    displayPoint(Board.door_position, Ds, '36').

/*
 * mapeamento dos símbolos de cada elemento do tabuleiro.
*/

wall_symbol('█').
player_symbol('𖦔').
bomb_symbol('δ').
box_symbol('▓').
explosion_symbol('𖤌').
key_symbol('⚷').
door_symbol('በ').
