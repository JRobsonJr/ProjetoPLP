:- use_module(library(apply)).
:- use_module(library(csv)).

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

 
compare_to(<,A,B) :- 
    nth0(1,A,X),
    nth0(1,B,Y), 
    X =< Y.
compare_to(>,_,_).

sort_by_score(SortedList) :-
    setup_players,
    findall([Player, Score], player(Player, Score), Queries),
    predsort(compare_to, Queries, List),
    reverse(List, SortedList).



write_word(Text, Theme):-
    get_level(Text, Level),
    string_lower(Text, TextLowerCase),
    string_lower(Theme, ThemeLowerCase),
    assertz(word(TextLowerCase, ThemeLowerCase, Level)),
    write_word_file.

write_player(Nickname, Score):-
    string_lower(Nickname, NicknameLowerCase),
    assertz(player(NicknameLowerCase, Score)),
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

random_fast_match:-
    setup_words,
    findall([Text, Theme, Level], word(Text, Theme, Level), Result),
    get_random_word(Result, RandomWord),
    writeln(RandomWord).
    % chamar startGame.

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
    get_char(_),
    clear_screen.

sleep_3s :-
    sleep(3).

exit :-
    halt.

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
    writeln("           Opção inválida... Pressione ENTER para tentar novamente!\n"),
    pause.
    
show_game_modes :-
    % clear_screen
    writeln("\n-----------------------------     MODO DE JOGO     -----------------------------\n\n"),
    writeln("                                1  -  Jogo Rápido"),
    writeln("                                2  -  Modo Campeonato"),
    writeln("                                3  -  Voltar").
    % get_option

show_hangman(Lives) :-
    clear_screen,
    writeln("                                 ###############"),
    writeln("                                 #### FORCA ####"),
    writeln("                                 ###############"),
    writeln("                                 #      |      #"),
    
    show_hangman_body(Lives),
    
    writeln("                                 ###############"),
    writeln("                                  /\\         /\\"),
    writeln("                                 /  \\       /  \\ \n").

show_hangman_body(7) :-
    writeln("                                 #             #"),
    writeln("                                 #             #"),
    writeln("                                 #             #"),
    writeln("                                 #             #").

show_hangman_body(6) :-
    writeln("                                 #    ('-')    #"),
    writeln("                                 #             #"),
    writeln("                                 #             #"),
    writeln("                                 #             #").

show_hangman_body(5) :-
    writeln("                                 #    ('-')__  #"),
    writeln("                                 #             #"),
    writeln("                                 #             #"),
    writeln("                                 #             #").

show_hangman_body(4) :-
    writeln("                                 #  __('-')__  #"),
    writeln("                                 #             #"),
    writeln("                                 #             #"),
    writeln("                                 #             #").

show_hangman_body(3) :-
    writeln("                                 #  __('-')__  #"),
    writeln("                                 #      |      #"),
    writeln("                                 #             #"),
    writeln("                                 #             #").

show_hangman_body(2) :-
    writeln("                                 #  __('-')__  #"),
    writeln("                                 #      |      #"),
    writeln("                                 #     /       #"),
    writeln("                                 #             #").

show_hangman_body(1) :-
    writeln("                                 #  __('-')__  #"),
    writeln("                                 #      |      #"),
    writeln("                                 #     / \\     #"),
    writeln("                                 #             #").
    
show_hangman_body(0) :-
    writeln("                                 #      |      #"),
    writeln("                                 #    (-.-)    #"),
    writeln("                                 #     /|\\     #"),
    writeln("                                 #     / \\     #").
    
show_victory_hangman :-
    writeln("                                 ###############"),
    writeln("                                 #### FORCA ####"),
    writeln("                                 ###############"),
    writeln("                                 #      |      #"),
    writeln("                                 #             #"),
    writeln("                                 #             #"),
    writeln("                                 #             #"),
    writeln("                                 #             #"),
    writeln("                                 ###############     \\('◡')/"),
    writeln("                                  /\\         /\\         |"),
    writeln("                                 /  \\       /  \\       / \\ \n\n").

show_defeat_hangman :-
    writeln("                                 ###############"),
    writeln("                                 #### FORCA ####"),
    writeln("                                 ###############"),
    writeln("                                 #      |      #"),
    writeln("                                 #      |      #"),
    writeln("                                 #    (-.-)    #"),
    writeln("                                 #     /|\\     #"),
    writeln("                                 #     / \\     #"),
    writeln("                                 ###############"),
    writeln("                                  /\\         /\\"),
    writeln("                                 /  \\       /  \\ \n").

fast_match_mode :-
    clear_screen,
    writeln("\n-----------------------------     JOGO RÁPIDO     ------------------------------\n\n"),
    writeln("                      Como sua palavra deve ser escolhida?\n"),
    writeln("                              1  -  Por Tema"),
    writeln("                              2  -  Por Dificuldade"),
    writeln("                              3  -  Aleatoriamente"),
    writeln("                              4  -  Voltar").
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

show_themes :-
    get_themes(Themes),
    print_themes(Themes, 1).

print_themes([], _).
print_themes([Head|Tail], Index) :-
    string_concat("                              ", Index, SpacesAndIndex),
    string_concat(SpacesAndIndex, "  -  ", SpacesIndexAndDash),
    string_concat(SpacesIndexAndDash, Head, CompleteString),
    writeln(CompleteString),
    Index1 is Index + 1,
    print_themes(Tail, Index1).

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
    
    writeln("\n                         [ Pressione ENTER para voltar ]\n\n"),
    pause.

get_word_data :-
    clear_screen,
    writeln("\n---------------------------     CADASTRAR PALAVRA     --------------------------\n\n\n"),
    write("          Informe a nova palavra: "),
    read(Word),

    writeln("\n                              Temas já cadastrados:"),
    show_themes,

    write("\n          Informe o tema da palavra (por extenso): "),
    read(Theme),
    get_word_data2(Word, Theme).

get_word_data2(Word, Theme) :-
    setup_words,
    word(Word, Theme, _),
    get_word_data_failure.
get_word_data2(Word, Theme) :-
    get_word_data_success(Word, Theme) .

get_word_data_success(Word, Theme) :-
    write_word(Word, Theme),
    writeln("\n\n                         Palavra cadastrada com sucesso!\n"),
    writeln("                                   Aguarde...\n\n"),
    sleep_3s.

get_word_data_failure :-
    writeln("                       Opa, essa palavra já foi cadastrada..."),
    writeln("                      [ Pressione ENTER para tentar novamente ]\n\n"),
    pause.

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
    sort_by_score(SortedList),
    writeln(SortedList).
