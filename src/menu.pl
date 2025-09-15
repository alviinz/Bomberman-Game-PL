:- module(menu, [run_menu/1]).

:- use_module(ansi_terminal).
:- use_module(gameLoop).

/*
Predicado principal que inicia o loop do menu.

@predicate run_menu(+Configs)
*/
run_menu(Configs) :-
    menu_loop(Configs).

/*
Mostra o menu, lê o input e trata da escolha do utilizador.
*/
menu_loop(Configs) :-
    display_menu,
    get_single_char(Code),
    char_code(Char, Code),
    handle_menu_choice(Char, Configs).

/*
Executa uma ação com base na escolha do utilizador.
*/
handle_menu_choice('1', Configs) :-
    init_game(Configs). 

handle_menu_choice('2', Configs) :-
    clearDisplay,
    get_dict(width, Configs, W),
    get_dict(height, Configs, H),
    MsgX is W // 2 - 5,
    MsgY is H // 2 + 1,
    writeAt(MsgX, MsgY, "NÃO TÁ AFIM DE JOGAR?", '1;93'),
    flush_output,
    sleep(1),
    writeAt(MsgX, MsgY + 1, "ATÉ A PROXIMA!!", '1;93'),
    movePointer(0, H + 2),
    showCursor,
    halt.

handle_menu_choice(_, Configs) :- 
    style_codes_to_string([red], Codes), 
    writeAt(10, 10, 'Opção inválida!', Codes),
    sleep(1.5),
    menu_loop(Configs). 

/*
Desenha o menu principal no ecrã.
*/
display_menu :-
    clearDisplay,
    hideCursor,
    X_Title is 25, Y_Title is 5,
    X_Options is 25, Y_Options is 8,

    style_codes_to_string([bold, yellow], TitleCodes),
    style_codes_to_string([yellow], SeparatorCodes),
    style_codes_to_string([white], OptionCodes1),
    style_codes_to_string([red], OptionCodes2),
    style_codes_to_string([bold, white], PromptCodes),

    writeAt(X_Title, Y_Title,     ' B O M B E R M A N ', TitleCodes),
    writeAt(X_Title, Y_Title + 1, '===================', SeparatorCodes),

    writeAt(X_Options, Y_Options,     ' [1] Jogar', OptionCodes1),
    writeAt(X_Options, Y_Options + 1, ' [2] Sair', OptionCodes2),

    writeAt(X_Options, Y_Options + 3, ' Escolha uma opção ', PromptCodes),
    flush_output.


% --- PREDICADOS AUXILIARES PARA CORES ---

/*
Converte uma lista de estilos (ex: [bold, red]) numa string de códigos ANSI (ex: '1;31').
*/
style_codes_to_string(Styles, CodesString) :-
    maplist(style_code, Styles, CodesList),
    atomic_list_concat(CodesList, ';', CodesString).

% Mapeamento dos nomes dos estilos para os seus códigos ANSI
style_code(bold, '1').
style_code(red, '31').
style_code(green, '32').
style_code(yellow, '33').
style_code(blue, '34').
style_code(magenta, '35').
style_code(cyan, '36').
style_code(white, '37').