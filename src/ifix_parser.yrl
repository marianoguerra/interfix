Nonterminals
    program tl_exprs tl_expr call
    call_block call_blocks call_block_1
    call_items call_item literal
    seq seq_items seq_item list map tuple.

Terminals
    atom kw var integer float string bstring
    nl colon dot semicolon split
    open_list close_list
    open_map close_map
    open_tuple
    open close.

Rootsymbol program.

program -> tl_exprs : '$1'.
program -> nl tl_exprs : '$2'.

tl_exprs -> tl_expr : ['$1'].
tl_exprs -> tl_expr nl tl_exprs : ['$1'|'$3'].

tl_expr -> call : '$1'.
tl_expr -> call_block : '$1'.

call -> call_items : {call, line('$1'), parse_call_items('$1')}.
call_block -> call_blocks dot : {cb, line('$1'), '$1'}.

call_block_1 -> call colon tl_exprs : {clause, line('$1'), {'$1', '$3'}}.

call_blocks -> call_block_1: ['$1'].
call_blocks -> call_block_1 semicolon: ['$1'].
call_blocks -> call_block_1 semicolon call_blocks : ['$1'|'$3'].

call_items -> call_item : ['$1'].
call_items -> call_item call_items : ['$1'|'$2'].

call_item -> split   : '$1'.
call_item -> atom    : '$1'.
call_item -> literal : '$1'.
call_item -> seq     : '$1'.

seq -> list : '$1'.
seq -> map : '$1'.
seq -> tuple: '$1'.

list -> open_list close_list : {list, line('$1'), []}.
list -> open_list seq_items close_list : {list, line('$1'), '$2'}.

tuple -> open_tuple close_list : {tuple, line('$1'), []}.
tuple -> open_tuple seq_items close_list : {tuple, line('$1'), '$2'}.

map -> open_map close_map : {map, line('$1'), []}.
map -> open_map seq_items close_map : {map, line('$1'), '$2'}.

seq_items -> seq_item : ['$1'].
seq_items -> seq_item seq_items : ['$1'|'$2'].

seq_item -> literal : '$1'.
seq_item -> seq : '$1'.

literal -> kw : '$1'.
literal -> var : '$1'.
literal -> integer : '$1'.
literal -> float: '$1'.
literal -> string : '$1'.
literal -> bstring : '$1'.
literal -> open call close : '$2'.
literal -> open call_blocks close : {cb, line('$2'), '$2'}.

Erlang code.

%unwrap({_,V})   -> V;
%unwrap({_,_,V}) -> V;
%unwrap(V) -> ct:print("WAT ~p", [V]).

line(T) when is_tuple(T) -> element(2, T);
line([H|_T]) -> element(2, H);
line(T) -> ct:print("WAT ~p", [T]).

parse_call_items(Items) -> parse_call_items(Items, [], []).

parse_call_items([], Names, Values) ->
    {lists:reverse(Names), lists:reverse(Values)};
parse_call_items([{atom, _L, V}|T], Names, Values) ->
    parse_call_items(T, [V|Names], Values);
parse_call_items([V={split, _L, _V}|T], Names, Values) ->
    parse_call_items(T, [V|Names], Values);
parse_call_items([H={_T, _L, _V}|T], Names, Values) ->
    parse_call_items(T, ['_'|Names], [H|Values]).
