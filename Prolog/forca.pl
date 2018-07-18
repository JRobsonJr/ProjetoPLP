:- use_module(library(apply)).
:- use_module(library(csv)).

read_data(File, Lists):-
    csv_read_file(File, Rows),
    rows_to_lists(Rows, Lists).

rows_to_lists(Rows, Lists):-
    maplist(row_to_list, Rows, Lists).

row_to_list(Row, List):-
    Row =.. [_|List].

setup_words(Data) :-
    read_data('../resources/words.csv', Data).

setup_players(Data) :-
    read_data('../resources/players.csv', Data).

get_themes(Themes):-
    setup_words(Data),
    get_themes_loop(Data, Themes).

get_themes_loop([], Themes):-!.
get_themes_loop([Head|Tail], Themes):-
    nth0(1, Head, X),
    (member(X, Themes) -> get_themes_loop(Tail, Themes);
    get_themes_loop(Tail, [X|Themes])).
    
:- initialization(main).
main :-
    get_themes(Themes),
    writeln(Themes).

