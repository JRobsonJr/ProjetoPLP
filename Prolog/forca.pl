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

sort_players_by_score(SortedList) :-
    setup_players,
    findall([Player, Score], player(Player, Score), Queries),
    predsort(compare_to, Queries, List),
    reverse(List, SortedList).

get_spaces(0, "").
get_spaces(Quantity, Result):-
	Quantity1 is Quantity-1,
	get_spaces(Quantity1, Result1),
	string_concat(" ", Result1, Result).

get_length_spacing(Size, SizeScore, Result):-
	compare(>, SizeScore, 2),
	Value is Size - (SizeScore - 2),
	get_spaces(Value, Result).

get_length_spacing(Size, _, Result):-
	get_spaces(Size, Result).

show_players([], 11, "").
show_players([], 10, Result):- 
	Result is "                       10º ------------       ---------\n".
show_players([], Index, Result):-
	string_concat("                        ", Index, Result1),
	string_concat(Result1, "º ------------       ---------\n", Result2),
	show_players([], Index+1, Result3),
	string_concat(Result2, Result3,Result).

show_players([Head|Tail], Index, Result):- 
	string_concat("                        ", Index, Result1),
	string_concat(Result1, "º ", Result2),
	nth0(0,Head,Name),
	nth0(1,Head,Score),
	atom_length(Name, SizeName),
	atom_length(Score, SizeScore),
	atom_string(Name, StringName),
	string_concat(Result2, StringName, Result3),
	Value is 23 - SizeName,
	get_length_spacing(Value, SizeScore, StringResult),
	string_concat(Result3, StringResult, Result4),
	atom_string(Score, StringScore),
	string_concat(Result4, StringScore, Result5),
	string_concat(Result5, "\n", Result6),
	Index1 is Index+1,
	show_players(Tail, Index1, Result7),
	string_concat(Result6, Result7, Result).

write_word(Text, Theme):-
    get_level(Text, Level),
    string_lower(Text, TextLowerCase),
    atom_string(TextAtom, TextLowerCase),
    string_lower(Theme, ThemeLowerCase),
    atom_string(ThemeAtom, ThemeLowerCase),
    assertz(word(TextAtom, ThemeAtom, Level)),
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
    write("\n\n                    Informe o número da opção desejada: "),
    read(Option). 

select_menu_option(1) :- show_game_modes.
select_menu_option(2) :- show_rules.
select_menu_option(3) :- show_ranking.
select_menu_option(4) :- get_word_data.
select_menu_option(5) :- quit.
select_menu_option(_) :- show_invalid_option_message.

clear_screen :-
    tty_clear.

pause :-
    get_char(_),
    get_char(_),
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
    
    writeln("                         [ Pressione ENTER para voltar ]\n\n\n"),
    pause,
    show_menu.

show_victory_message :-
    clear_screen,
    writeln("                     Parabéns, você acaba de salvar uma vida!\n\n"),
    show_victory_hangman.

show_game_over_message :-
    clear_screen,
    writeln("                       É realmente uma pena, fim de jogo...\n\n"),
    show_defeat_hangman.

show_ranking:-
	clear_screen,
	writeln("\n--------------------------------     RANKING     -------------------------------\n\n\n"),
	writeln("                             Jogador          Pontuação\n"),
	sort_players_by_score(SortedList),
	show_players(SortedList, 1, Result),
	writeln(Result),
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
    get_word_data2(Word, Theme),
    show_menu.

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

get_hidden_word(Word, HiddenWord):-
    string_chars(Word, CharList),
    get_hidden_word_chars(CharList, HiddenWordChars),
    string_chars(HiddenWord, HiddenWordChars).

get_hidden_word_chars([], []).
get_hidden_word_chars([' '|Tail], HiddenWordChars):-
    get_hidden_word_chars(Tail, HiddenWordCharsAux),
    HiddenWordChars = [' '|HiddenWordCharsAux].
get_hidden_word_chars([Head|Tail], HiddenWordChars):-
    Head \= ' ',
    get_hidden_word_chars(Tail, HiddenWordCharsAux),
    HiddenWordChars = ['_'|HiddenWordCharsAux].

get_score(_, 0, _, 0):-!.
get_score(Word, Lives, HintsUsed, Score):-
    setup_words,
    word(Word, _, Level),
    string_length(Word, Length),
    Score is Length * Level * Lives + 50 * Level - 25 * HintsUsed.

reveal_letter(Word, HiddenWord, Letter, Result):-
    string_chars(Word, WordChars),
    string_chars(HiddenWord, HiddenWordChars),
    reveal_letter_chars(WordChars, HiddenWordChars, Letter, ResultChars),
    string_chars(Result, ResultChars).

reveal_letter_chars([], [], _, []).
reveal_letter_chars([Letter|WordTail], ['_'|HiddenWordTail], Letter, ResultChars):-
    reveal_letter_chars(WordTail, HiddenWordTail, Letter, ResultCharsAux),
    ResultChars = [Letter|ResultCharsAux].
reveal_letter_chars([_|WordTail], [HiddenWordHead|HiddenWordTail], Letter, ResultChars):-
    reveal_letter_chars(WordTail, HiddenWordTail, Letter, ResultCharsAux),
    ResultChars = [HiddenWordHead|ResultCharsAux].

show_guesses([], '').
show_guesses([GuessesHead], GuessesHead).
show_guesses([GuessesHead|GuessesTail], Result):-
    show_guesses(GuessesTail, ResultAux),
    string_add_space(GuessesHead, GuessesHeadWithSpace),
    string_concat(GuessesHeadWithSpace, ResultAux, Result).

string_add_space(String, StringWithSpace):-
    string_concat(String, ' ', StringWithSpace).

guess_letter(Word, Guesses, Result):-
    writeln("Digite uma letra: "),
    get_char(Letter),
    get_char(_),
    guess_letter_aux(Word, Guesses, Letter, Result).

guess_letter_aux(Word, Guesses, Letter, Result):-
    member(Letter, Guesses),
    writeln("Essa letra já foi sugerida. Tente outra!"),
    guess_letter(Word, Guesses, Result), !.

guess_letter_aux(Word, Guesses, Letter, Letter):-
    is_alpha(Letter).

guess_letter_aux(Word, Guesses, Letter, Result):-
    \+ is_alpha(Letter),
    writeln("Uma letra, meu anjo..."),
    guess_letter(Word, Guesses, Result).

start_game(Word, Score):-
    get_hidden_word(Word, HiddenWord),
    run_game(Word, HiddenWord, [], 7, 0, Score).

run_game(Word, HiddenWord, Guesses, 0, HintsUsed, 0):-
    show_game_over_message,
    reveal_word(Word), !.

run_game(Word, HiddenWord, Guesses, Lives, HintsUsed, Score):-
    all_letters_revealed(HiddenWord),
    get_score(Word, Lives, HintsUsed, Score),
    show_victory_message,
    reveal_word(Word), !.

run_game(Word, HiddenWord, Guesses, Lives, HintsUsed, Score):-
    show_game_info(Word, HiddenWord, Guesses, Lives, HintsUsed),
    guess_letter(Word, Guesses, Letter),
    reveal_letter(Word, HiddenWord, Letter, HiddenWordAux),
    get_lives(HiddenWord, HiddenWordAux, Lives, LivesAux),
    run_game(Word, HiddenWordAux, [Letter|Guesses], LivesAux, HintsUsed, Score).

get_lives(HiddenWord, HiddenWordAux, CurrentLives, Lives):-
    HiddenWord == HiddenWordAux -> 
        Lives is CurrentLives - 1;
        Lives is CurrentLives.

all_letters_revealed(Word):-
    string_chars(Word, Chars),
    \+ member('_', Chars).

show_game_info(Word, HiddenWord, Guesses, Lives, HintsUsed):-
    show_hangman(Lives),
    setup_words,
    word(Word, Theme, _),
    write("Tema: "), writeln(Theme),
    write("Palavra: "), writeln(HiddenWord),
    write("Letras já usadas: "),
    show_guesses(Guesses, GuessesString),
    writeln(GuessesString),
    write("Dicas usadas: "), writeln(HintsUsed).

reveal_word(Word):-
    write("\nA palavra era: "), write(Word),
    write(".\n\n                         [ Pressione ENTER para voltar ]"),
    get_char(_).

get_tip(Word, Guesses, Letter):-
    atom_string(Word, StringWord),
    string_chars(Word, Chars),
    length(Chars, Size),
    random(0, Size, RandomIndex),
    nth0(RandomIndex, Chars, RandomLetter),
    get_tip_aux(RandomLetter, Guesses, Letter).

get_tip_aux(RandomLetter, Guesses, Letter):-
    member(RandomLetter, Guesses),
    get_tip(Word, Guesses, Letter).

get_tip_aux(RandomLetter, Guesses, RandomLetter).


:- initialization(main).

main:-
	show_opening,   
    show_menu.
