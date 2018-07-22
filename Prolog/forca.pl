:- use_module(library(apply)).
:- use_module(library(csv)).

write_data(File, Data):-
    open(File, append, Stream),
    write(Stream, Data),
    nl(Stream),
    close(Stream).

read_data(File, Lists):-
    csv_read_file(File, Rows),
    rows_to_lists(Rows, Lists).

rows_to_lists(Rows, Lists):-
    maplist(row_to_list, Rows, Lists).

row_to_list(Row, List):-
    Row =.. [_|List].

setup_words(Data) :-
    read_data('../resources/words_lower_case.csv', Data).

setup_players(Data) :-
    read_data('../resources/players.csv', Data).

write_word(Text, Theme):-
    get_level(Text, Level),
    atomic_list_concat([Text, Theme, Level], ',', Data),
    write_data('../resources/words_lower_case.csv', Data).

write_player(Nickname, Score):-
    atomic_list_concat([Nickname, Score], ',', Data),
    write_data('../resources/players.csv', Data).

get_themes(Result):-
    setup_words(Data),
    get_themes_loop(Data, [], Result).

% Adiciona os temas no inicio da lista
get_themes_loop([], Themes, Themes).
get_themes_loop([Head|Tail], Themes, Result):-
    nth0(1, Head, Elem),
    (member(Elem, Themes) -> get_themes_loop(Tail, Themes, Result);
    get_themes_loop(Tail, [Elem|Themes], Result)).

get_level(Text, Level):- 
    atom_length(Text, Size),
    (compare(<, Size, 6) -> Level is 1;
    compare(<, Size, 10) -> Level is 2;
    Level is 3).
    
filter_words(Criteria, Result):-
    setup_words(Data),
    filter_words_loop(Data, Criteria, [], Result).

filter_words_loop([], _, Result, Result).
filter_words_loop([Head|Tail], Criteria, List, Result):-
    member(Criteria, Head) -> 
        append(List, [Head], NewList), 
        filter_words_loop(Tail, Criteria, NewList, Result);
    filter_words_loop(Tail, Criteria, List, Result).

themed_fast_match:- 
    % lower_case
    % chamar metodo com a interface(select_theme),
    read(Theme),
    filter_words(Theme, Result),
    get_random_word(Result, RandomWord),
    writeln(RandomWord).
    % chamar startGame.

leveled_fast_match:-
    % chamar select_level
    read(Level),
    filter_words(Level, Result),
    get_random_word(Result, RandomWord),
    writeln(RandomWord).
    %chamar startGame.
    
get_random_word(Words, RandomWord):-
    length(Words, Size),
    random(0, Size, RandomIndex),
    nth0(RandomIndex, Words, RandomWord).

show_opening :-
    writeln("      ____________..___________                                                 "),
    writeln("     | .___________))__________|                                                "),
    writeln("     | | / /       ||                                                           "),
    writeln("     | |/ /        ||                          _                                "),
    writeln("     | | /         ||.-''.                    | |                               "),
    writeln("     | |/          |/  _  \\                   | | ___   __ _  ___              "),
    writeln("     | |           ||  `/,|               _   | |/ _ \\ / _` |/ _ \\            "),
    writeln("     | |           (\\\\`_.'               | |__| | (_) | (_| | (_) |           "),
    writeln("     | |          .-`--'.                 \\____/ \\___/ \\___ |\\___/          "),
    writeln("     | |         /Y . . Y\\                              __/ |                  "),
    writeln("     | |        // |   | \\\\                            |___/                  "),
    writeln("     | |       //  | . |  \\\\                                                  "),
    writeln("     | |      ')   | _ |   (`         _           ______                        "),
    writeln("     | |           || ||             | |         |  ____|                       "),
    writeln("     | |           || ||           __| | __ _    | |__ ___  _ __ ___ __ _       "),
    writeln("     | |           || ||          / _` |/ _` |   |  __/ _ \\| '__/ __/ _` |     "),
    writeln("     | |           || ||         | (_| | (_| |   | | | (_) | | | (_| (_| |      "),
    writeln("     | |          / | | \\         \\____|\\____|   |_|  \\___/|_|  \\___\\____|"),
    writeln("     | |          `-' `-'                                                       "),
    writeln("     |_|                                                                        "),
    writeln("                                   Aguarde...                                   ").
    % wait

show_menu :-
    % clear_screen
    writeln("\n---------------------------------     MENU     ---------------------------------\n\n"),
    writeln("                                1  -  Jogar"),
    writeln("                                2  -  Regras"),
    writeln("                                3  -  Ranking"),
    writeln("                                4  -  Nova Palavra"),
    writeln("                                5  -  Sair").
    % get_option
    
show_invalid_option_message :-
    writeln("           Opção inválida... Pressione ENTER para tentar novamente!\n").
    % pause
    

show_game_modes :-
    % clear_screen
    writeln("\n-----------------------------     MODO DE JOGO     -----------------------------\n\n"),
    writeln("                                1  -  Jogo Rápido"),
    writeln("                                2  -  Modo Campeonato"),
    writeln("                                3  -  Voltar").
    % get_option

fast_match_mode :-
    % clearScreen
    writeln("\n-----------------------------     JOGO RÁPIDO     ------------------------------\n\n"),
    writeln("                      Como sua palavra deve ser escolhida?\n"),
    writeln("                              1  -  Por Tema"),
    writeln("                              2  -  Por Dificuldade"),
    writeln("                              3  -  Aleatoriamente"),
    writeln("                              4  -  Voltar").
    % get_option

select_theme :-
    % clearScreen
    writeln("\n----------------------------     SELECIONAR TEMA     ---------------------------\n\n").
    % show_themes
    % get_option

show_levels :-
    % clear_screen
    writeln("\n------------------------     SELECIONAR DIFICULDADE     ------------------------\n\n"),
    writeln("                              1  -  Fácil"),
    writeln("                              2  -  Médio"),
    writeln("                              3  -  Difícil\n\n").
    % get_option

show_rules :- 
    % clear_screen
    writeln("\n--------------------------------     REGRAS     --------------------------------\n\n\n"),
    
    writeln("    No jogo da forca, o jogador deve acertar a palavra que lhe foi proposta a pa"),
    writeln("rtir do número de letras dessa palavra e do tema ligado a ela. A cada rodada, o "),
    writeln("jogador deve sugerir uma letra. Se a palavra proposta contiver a letra sugerida,"),
    writeln(" todas as ocorrências dessa letra são reveladas. Caso contrário, uma parte do en"),
    writeln("forcado será desenhada. Para vencer, o jogador deve ser capaz de revelar a palav"),
    writeln("ra por completo antes que todo o corpo do enforcado seja desenhado.\n"),
    
    writeln("    O jogador poderá se desafiar em dois modos distintos: o rápido e o campeonat"),
    writeln("o. No modo rápido, o jogador enfrentará apenas uma palavra, podendo especificar "),
    writeln("a dificuldade ou o tema dela, se assim desejar. Já no modo campeonato, o jogador"),
    writeln("enfrentará diversas palavras em sequência e acumulará pontos a cada palavra reve"),
    writeln("lada. Nesse modo, o jogo segue até que o jogador perca uma partida ou adivinhe "),
    writeln("todas as palavras possíveis. Em qualquer um dos casos, seu desempenho será regis"),
    writeln("trado no ranking.\n\n\n"),
    
    writeln("                         [ Pressione ENTER para voltar ]\n\n\n").
    % pause

show_victory_message :-
    % clear_screen
    writeln("                     Parabéns, você acaba de salvar uma vida!\n\n").
    % show_victory_hangman

show_game_over_message :-
    % clear_screen
    writeln("                       É realmente uma pena, fim de jogo...\n\n").
    % show_defeat_hangman
    
show_ranking :-
    % clear_screen
    writeln("\n--------------------------------     RANKING     -------------------------------\n\n\n"),
    writeln("                             Jogador          Pontuação\n"),
    
    % ranking
    
    writeln("\n                         [ Pressione ENTER para voltar ]\n\n").
    % pause

quit :-
    % clear_screen
    writeln("\n\n                                 Até a próxima!"),
    writeln("\n\n             Paradigmas de Linguagem de Programação - 2018.1 - UFCG"),
    writeln("\n\n                                DESENVOLVIDO POR:\n"),
    writeln("                              Fanny Batista Vieira"),
    writeln("                       José Robson da Silva Araujo Junior"),
    writeln("                            Matheus Alves dos Santos"),
    writeln("                         Misael Augusto Silva da Costa"), 
    writeln("                            Paulo José Bastos Leitão\n\n").
    % wait

:- initialization(main).
main:-
    themed_fast_match.
    
