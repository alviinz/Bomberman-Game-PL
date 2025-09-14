:- module(ansi_terminal, [clearDisplay/0,
                          movePointer/2,
                          writeAt/3,
                          writeAt/4,
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
Personaliza o texto seguinte do terminal.

@predicade movePointer(+Codes)

@param Codes é uma string que contém o(s) código(s) separados por ';' para personalização do terminal.

Os principais códigos são:

--- ESTILOS DE TEXTO ---
0   - Reset / Normal      (Remove toda a formatação)
1   - Negrito             (Texto mais espesso ou brilhante)
4   - Sublinhado          (Adiciona linha abaixo do texto)
5   - Piscando            (Faz o texto piscar)
7   - Inverter Cores      (Inverte cores do texto e fundo)

--- CORES DE TEXTO ---
Cor        Padrão   Brilhante
Preto      30       90
Vermelho   31       91
Verde      32       92
Amarelo    33       93
Azul       34       94
Magenta    35       95
Ciano      36       96
Branco     37       97

--- CORES DE FUNDO ---
Cor        Padrão   Brilhante
Preto      40       100
Vermelho   41       101
Verde      42       102
Amarelo    43       103
Azul       44       104
Magenta    45       105
Ciano      46       106
Branco     47       107

@example
    personalizeTerm('32').      %% a cor do texto será verde.
    personalizeTerm('41').      %% a cor de fundo do texto será vermelho.
    personalizeTerm('32;41').   %% a cor do texto será verde e a cor de fundo será vermelha.
    personalizeTerm('1;32;41'). %% o texto estará em negrito, sua cor verde e a cor de fundo vermelha.
*/
personalizeTerm(Codes) :-
    format('\033[~am', [Codes]).

/*
Reseta toda a personalização do terminal.

@predicade resetTerm.
*/
resetTerm() :-
    personalizeTerm('0').

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
Realiza a mesma função do writeAt/3, com a adição de mudar a cor do texto.

@predicade movePointer(+X, +Y, +Text, +Codes)

@param X     é a posição da coluna.
@param Y     é a posição da linha.
@param Text  é a string ou charactere que deseja escrever no terminal.
@param Codes são os códigos de personalização do terminal no formato do ANSI. (veja a documentação do colorTerminal para consultar os códigos de cores)
*/
writeAt(X, Y, Text, Codes) :-
    personalizeTerm(Codes),
    writeAt(X, Y, Text),
    resetTerm.

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
