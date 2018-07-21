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

:- initialization(main).
main:-
    themed_fast_match.
    
