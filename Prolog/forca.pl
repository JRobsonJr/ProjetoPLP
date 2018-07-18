:- use_module(library(apply)).
:- use_module(library(csv)).

read_data(File, Lists):-
    csv_read_file(File, Rows, []),
    rows_to_lists(Rows, Lists).

rows_to_lists(Rows, Lists):-
    maplist(row_to_list, Rows, Lists).

row_to_list(Row, List):-
    Row =.. [_|List].

setup_words(Data) :-
    get_rows_data('../resources/words.csv', Data).

setup_players(Data) :-
    get_rows_data('../resources/players.csv', Data).

:- initialization(main).
main :-
    setup_players(X),
    writeln(X),
    setup_words(Y),
    writeln(Y).