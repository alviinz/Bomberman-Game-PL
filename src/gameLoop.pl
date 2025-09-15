:- module(gameLoop, [init_game/1]).
:- use_module(board).
:- use_module(player).
:- use_module(display_board).
:- use_module(display_common).
:- use_module(ansi_terminal).
:- use_module(utils).

/*
inicia um jogo com um conjunto de configurações (Configs).

@predicate init_game(+Configs).

@param Configs uma dict contendo as configurações do jogo.
*/
init_game(Configs) :-
    hideCursor,
    createBoard(Configs, Board),
    gameLoop(Configs, Board),
    showCursor.

/*
O loop de uma partida. Representa, portanto, a execução de uma única partida do jogo.

@predicate gameLoop(+Configs, +Board).

@param Configs uma dict representando as configurações do jogo.
@param Board   uma dict representando o tabuleiro.

*/
gameLoop(Configs, Board) :-
    displayBoard(Board),
    % LINHA ADICIONADA: O laço agora verifica se o jogo terminou
    (Board.game_over ->
        exitDisplay(Board)
    ;
    get_single_char(Code),
    char_code(Char, Code),
    (Char = 'q';is_dead(Board) ->
        exitDisplay(Board)
    ;
        (Char = ' ' ->
            placeBombs(Board, AlmostBoard)
        ;
            movePlayer(Char, Board.player, NewPlayer),
            updateBoard(Board, NewPlayer, AlmostBoard)
        ),
        update_bombs_and_create_explosions(AlmostBoard, BoardWithNewExplosions),
        update_existing_explosions(BoardWithNewExplosions, BoardWithUpdatedExplosions),
        % A lógica de verificação de vitória é chamada aqui
        check_win_condition(BoardWithUpdatedExplosions, FinalBoard),
        gameLoop(Configs, FinalBoard)
    )).
    
/*
Verifica as condições de vitória do jogo, como a coleta da chave ou a chegada à porta.

@predicate check_win_condition(+Board, -NewBoard).

@param Board O tabuleiro atual do jogo, representado por um dicionário.
@param NewBoard O novo estado do tabuleiro após a verificação das condições.

@return Retorna NewBoard com o campo 'has_key' como true se a chave foi coletada,
        ou 'game_over' como true se o jogador alcançou a porta com a chave.
        Caso contrário, retorna o tabuleiro inalterado.
*/
check_win_condition(Board, NewBoard) :-
    (Board.player =@= Board.key_position, \+ Board.has_key) ->
        NewBoard = Board.put(has_key, true)
    ;
    %  Verifica a condição da porta e do  game_over
    (Board.player =@= Board.door_position, Board.has_key) ->
        NewBoard = Board.put(game_over, true)
    ;
    
    NewBoard = Board.  
