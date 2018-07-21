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

get_level(Text, Level):- 
    atom_length(Text, Size),
    (compare(<, Size, 6) -> Level is 1;
    compare(<, Size, 10) -> Level is 2;
    Level is 3).

get_themes(Result):-
    setup_words(Data),
    get_themes_loop(Data, [], Result).

% Adiciona os temas no inicio da lista
get_themes_loop([], Themes, Themes).
get_themes_loop([Head|Tail], Themes, Result):-
    nth0(1, Head, Elem),
    member(Elem, Themes) -> get_themes_loop(Tail, Themes, Result);
    get_themes_loop(Tail, [Elem|Themes], Result).
    

filter_by_theme(Theme, Result):-
    setup_words(Data),
    filter_by_theme_loop(Data, Theme, [], Result).

filter_by_theme_loop([], _, Result, Result).
filter_by_theme_loop([Head|Tail], Theme, List, Result):-
    member(Theme, Head) -> 
        append(List, Head, NewList), 
        filter_by_theme_loop(Tail, Theme, NewList, Result);
    filter_by_theme_loop(Tail, Theme, List, Result).

:- initialization(main).
main :-
    write_player('Fanny', 50).
