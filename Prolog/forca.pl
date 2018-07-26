:- use_module(library(apply)).
:- use_module(library(csv)).
:- use_module(library(tty)).

write_word_file :-
    setup_words,
    tell('words.pl'),nl,
    listing(word/3),
    told.

write_player_file :-
    setup_players,
    tell('players.pl'),nl,
    listing(player/2),
    told.

setup_words :-
    reconsult('words.pl').

setup_players :-
    reconsult('players.pl').

write_word(Text, Theme):-
    get_level(Text, Level),
    assertz(word(Text, Theme, Level)),
    write_word_file.

write_player(Nickname, Score):-
    assertz(player(Nickname, Score)),
    write_player_file.

get_themes(Result):-
    setup_words,
    findall(Theme, word(_, Theme, _), Queries),
    list_to_set(Queries, Result).

get_level(Text, Level):- 
    atom_length(Text, Size),
    (compare(<, Size, 6) -> Level is 1;
    compare(<, Size, 10) -> Level is 2;
    Level is 3).
    
filter_by_theme(Theme, Result):-
    setup_words,
    findall([Text, Theme, Level], word(Text, Theme, Level), Result).

filter_by_level(Level,Result):-
    setup_words,
    findall([Text, Theme, Level], word(Text, Theme, Level), Result).

themed_fast_match:- 
    % lower_case
    % chamar metodo com a interface(select_theme),
    read(Theme),
    filter_by_theme(Theme, Result),
    get_random_word(Result, RandomWord),
    writeln(RandomWord).
    % chamar startGame.

leveled_fast_match:-
    % chamar select_level
    read(Level),
    filter_by_level(Level, Result),
    get_random_word(Result, RandomWord),
    writeln(RandomWord).
    %chamar startGame.
    
get_random_word(Words, RandomWord):-
    length(Words, Size),
    random(0, Size, RandomIndex),
    nth0(RandomIndex, Words, RandomWord).
    
get_option(Option) :-
    writeln("\n\n                    Informe o número da opção desejada: "),
    read(Option). 

clear_screen :-
    tty_clear.

pause :-
    get_char(X),
    clear_screen.

sleep_3s :-
    sleep(3).

exit :-
    halt.

show_opening :-
    clear_screen,
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
    writeln("                                   Aguarde...                                   "),
    sleep_3s.

show_menu :-
    clear_screen,
    writeln("\n---------------------------------     MENU     ---------------------------------\n\n"),
    writeln("                                1  -  Jogar"),
    writeln("                                2  -  Regras"),
    writeln("                                3  -  Ranking"),
    writeln("                                4  -  Nova Palavra"),
    writeln("                                5  -  Sair"),
    get_option(Option),
    select_menu_option(Option).

select_menu_option(1) :- show_game_modes.
select_menu_option(2) :- show_rules.
select_menu_option(3) :- show_ranking.
select_menu_option(4) :- get_word_data.
select_menu_option(5) :- quit.
select_menu_option(_) :- show_invalid_option_message.

show_invalid_option_message :-
    writeln("           Opção inválida... Pressione ENTER para tentar novamente!\n").
    % pause
    

show_game_modes :-
    clear_screen,
    writeln("\n-----------------------------     MODO DE JOGO     -----------------------------\n\n"),
    writeln("                                1  -  Jogo Rápido"),
    writeln("                                2  -  Modo Campeonato"),
    writeln("                                3  -  Voltar").
    % get_option

fast_match_mode :-
    clear_screen,
    writeln("\n-----------------------------     JOGO RÁPIDO     ------------------------------\n\n"),
    writeln("                      Como sua palavra deve ser escolhida?\n"),
    writeln("                              1  -  Por Tema"),
    writeln("                              2  -  Por Dificuldade"),
    writeln("                              3  -  Aleatoriamente"),
    writeln("                              4  -  Voltar").
    % get_option

select_theme :-
    clear_screen,
    writeln("\n----------------------------     SELECIONAR TEMA     ---------------------------\n\n").
    % show_themes
    % get_option

show_levels :-
    clear_screen,
    writeln("\n------------------------     SELECIONAR DIFICULDADE     ------------------------\n\n"),
    writeln("                              1  -  Fácil"),
    writeln("                              2  -  Médio"),
    writeln("                              3  -  Difícil\n\n").
    % get_option

show_rules :- 
    clear_screen,
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
    
    writeln("                         [ Pressione ENTER para voltar ]\n\n\n"),
    pause.

show_victory_message :-
    clear_screen,
    writeln("                     Parabéns, você acaba de salvar uma vida!\n\n").
    % show_victory_hangman

show_game_over_message :-
    clear_screen,
    writeln("                       É realmente uma pena, fim de jogo...\n\n").
    % show_defeat_hangman
    
show_ranking :-
    clear_screen,
    writeln("\n--------------------------------     RANKING     -------------------------------\n\n\n"),
    writeln("                             Jogador          Pontuação\n"),
    
    % ranking
    
    writeln("\n                         [ Pressione ENTER para voltar ]\n\n").
    pause.

get_word_data :-
    clear_screen,
    writeln("\n---------------------------     CADASTRAR PALAVRA     --------------------------\n\n\n"),
    write("          Informe a nova palavra: "),
    read(Word),
    writeln("\n                              Temas já cadastrados:"),
    % show_themes
    write("\n          Informe o tema da palavra (por extenso): "),
    read(Theme),
    write_word(Word, Theme).

quit :-
    clear_screen,
    writeln("\n\n                                 Até a próxima!"),
    writeln("\n\n             Paradigmas de Linguagem de Programação - 2018.1 - UFCG"),
    writeln("\n\n                                DESENVOLVIDO POR:\n"),
    writeln("                              Fanny Batista Vieira"),
    writeln("                       José Robson da Silva Araujo Junior"),
    writeln("                            Matheus Alves dos Santos"),
    writeln("                         Misael Augusto Silva da Costa"), 
    writeln("                            Paulo José Bastos Leitão\n\n"),
    sleep_3s,
    exit.

:- initialization(main).
main:-
    show_menu.
