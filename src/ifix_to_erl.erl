-module(ifix_to_erl).
-export([to_erl/1]).

-include("../include/ifix.hrl").

to_erl(Ast) -> to_erl(Ast, #{errors => [], warnings => [], exported => [],
                             level => 0}).

to_erl(Ast, State) -> to_erl(Ast, [], State).

to_erl([], Accum, State) -> {lists:reverse(Accum), State};
to_erl([H|T], Accum, State) ->
    case convert(H, State) of
        {ok, R, State1} -> to_erl(T, [R|Accum], State1);
        {error, Error, State1} -> to_erl(T, Accum, add_error(State1, H, Error))
    end.

add_error(State=#{errors := Errors}, Ast, Error) ->
    State#{errors => [#{error => Error, code => Ast}|Errors]}.


convert_fn_clauses(Clauses=[?CL(?C([_Fn|FName], Args))|_], State) ->
    Arity = length(Args),
    convert_fn_clauses(Clauses, FName, Arity, [], State).

convert_fn_clauses([], Name, Arity, Accum, State) ->
    {ok, to_name(Name), Arity, lists:reverse(Accum), State};
convert_fn_clauses([?CL(Line, ?C([_Fn|FName], CArgs), Body)|T], Name, Arity, Accum, State) ->
    CurArity = length(CArgs),
    if CurArity /= Arity -> {error, head_arity_mismatch, State};
       FName /= Name -> {error, head_name_mismatch, State};
       true -> 
           {EBody, State1} = to_erl(Body, [], State),
           {EArgs, State2} = to_erl(CArgs, [], State1),
           EClause = {clause, Line, EArgs, [], EBody},
           convert_fn_clauses(T, Name, Arity, [EClause|Accum], State2)
    end.


convert(?CB(Line, Clauses=[?CL(?C([Fn|_FName]))|_]), State=#{level := Level}) 
        when Fn == 'fn'; Fn == 'fn+' ->
    if Level == 0 ->
           Exported = Fn == 'fn+',
           case convert_fn_clauses(Clauses, State) of
               {ok, [Name], Arity, EClauses, State1} ->
                   % TODO: for now export all
                   State2 = if Exported -> add_export(Name, Arity, State1);
                               true -> State1
                            end,
                   {ok, {function, Line, Name, Arity, EClauses}, State2};
               {error, Error, State1} ->
                   {error, Error, State1}
           end;
       true -> {error, fn_not_on_top_level, State}
    end;



convert(?CB(Line, Clauses=[?CL(?C(['when'|_FName]))|_]), State) ->
    case check_clauses(Clauses, 'when', true) of
        ok ->
            {EClauses, State1} = with_clauses(Clauses, State,
                                              fun convert_when_clauses/6, []),
            R = {'if', Line, EClauses},
            {ok, R, State1};
        {error, Reason} ->
            {error, {bad_expr, 'when', Reason}, State}
    end;

convert(V={var, _, _}, State) -> {ok, V, State};
convert(V={integer, _, _}, State) -> {ok, V, State};
convert(V={float, _, _}, State) -> {ok, V, State};
convert(V={string, _, _}, State) -> {ok, V, State};
convert({kw, Line, Val}, State) -> {ok, {atom, Line, Val}, State};
convert({list, Line, Val}, State) ->
    {R, State1} = list_to_cons_list(Line, Val, State),
    {ok, R, State1};
convert({tuple, Line, Val}, State) ->
    {EVal, State1} = to_erl(Val, [], State),
    {ok, {tuple, Line, EVal}, State1};
convert(?Op(Line, Op='+', Left, Right), State)  -> op(Line, Op, Left, Right, State);
convert(?Op(Line, Op='-', Left, Right), State)  -> op(Line, Op, Left, Right, State);
convert(?Op(Line, Op='*', Left, Right), State)  -> op(Line, Op, Left, Right, State);
convert(?Op(Line, Op='/', Left, Right), State)  -> op(Line, Op, Left, Right, State);
convert(?Op(Line, Op='//', Left, Right), State) -> op(Line, Op, Left, Right, State);
convert(?Op(Line, Op='%', Left, Right), State)  -> op(Line, Op, Left, Right, State);

convert(?Op(Line, Op='&', Left, Right), State)  -> op(Line, Op, Left, Right, State);
convert(?Op(Line, Op='^', Left, Right), State)  -> op(Line, Op, Left, Right, State);
convert(?Op(Line, Op='|', Left, Right), State)  -> op(Line, Op, Left, Right, State);

convert(?Op(Line, Op='<<', Left, Right), State) -> op(Line, Op, Left, Right, State);
convert(?Op(Line, Op='>>', Left, Right), State) -> op(Line, Op, Left, Right, State);

convert(?Op(Line, Op='and', Left, Right), State)-> op(Line, Op, Left, Right, State);
convert(?Op(Line, Op='or', Left, Right), State) -> op(Line, Op, Left, Right, State);
convert(?Op(Line, Op='xor', Left, Right), State)-> op(Line, Op, Left, Right, State);

convert(?Op(Line, Op='++', Left, Right), State) -> op(Line, Op, Left, Right, State);
convert(?Op(Line, Op='--', Left, Right), State) -> op(Line, Op, Left, Right, State);

convert(?Op(Line, Op='<', Left, Right), State)  -> op(Line, Op, Left, Right, State);
convert(?Op(Line, Op='>', Left, Right), State)  -> op(Line, Op, Left, Right, State);
convert(?Op(Line, Op='>=', Left, Right), State) -> op(Line, Op, Left, Right, State);
convert(?Op(Line, Op='<=', Left, Right), State) -> op(Line, Op, Left, Right, State);
convert(?Op(Line, Op='==', Left, Right), State) -> op(Line, Op, Left, Right, State);
convert(?Op(Line, Op='!=', Left, Right), State) -> op(Line, Op, Left, Right, State);

convert(?Op(Line, Op='not', Left), State) -> op(Line, Op, Left, State);
convert(?Op(Line, Op='~', Left), State) -> op(Line, Op, Left, State);

convert(?Op(Line, is, Left, Right), State) ->
    with_converted([Left, Right], State,
                   fun (State1, ELeft, ERight) ->
                           {ok, {match, Line, ELeft, ERight}, State1}
                   end);

convert(?C(['_'], [V]), State) ->
    convert(V, State);
convert(?C(Line, [call|Names], [{list, _, Args}]), State) ->
    Name = to_erl_call_name(Line, Names),
    {EArgs, State1} = to_erl(Args, [], State),
    {ok, {call, Line, Name, EArgs}, State1};
convert(?C(Line, Names=[First|_], Args), State) when First /= '_' ->
    Name = to_call_name(Line, Names),
    {EArgs, State1} = to_erl(Args, [], State),
    {ok, {call, Line, Name, EArgs}, State1};

convert(_, State) ->
    {error, unknown_node, State}.

to_erl_call_name(Line, Names) ->
    to_call_name(Line, lists:filter(fun ('_') -> false; (_) -> true end,
                                    Names)).

to_call_name(Line, Names) ->
    case to_name(Names) of
        [FName] ->
            {atom, Line, FName};
        [MName, FName] ->
            {remote, Line, {atom, Line, MName}, {atom, Line, FName}}
    end.

token_to_name('_') -> "O";
token_to_name(T) -> atom_to_list(T).

drop_tail_underscores(Items) ->
    lists:reverse(lists:dropwhile(fun ('_') -> true;
                                      (_) -> false
                                  end, lists:reverse(Items))).

to_name(Items) -> to_name(drop_tail_underscores(Items), [], []).

to_name([], CurAccum, Accum) ->
    lists:reverse(add_name_part(CurAccum, Accum));
to_name([{split, _, _}|T], CurAccum, Accum) ->
    to_name(T, [], add_name_part(CurAccum, Accum));
to_name([H|T], CurAccum, Accum) ->
    to_name(T, [H|CurAccum], Accum).

add_name_part([], Accum) -> Accum;
add_name_part(CurAccum, Accum) ->
    StrItems = lists:map(fun token_to_name/1, lists:reverse(CurAccum)),
    CurName = list_to_atom(string:join(StrItems, "_")),
    [CurName|Accum].

op(Line, Op, Left, Right, State) ->
    with_converted([Left, Right], State,
                   fun (State1, ELeft, ERight) ->
                           {ok, {op, Line, map_op(Op), ELeft, ERight}, State1}
                   end).

op(Line, Op, Left, State) ->
    with_converted([Left], State,
                   fun (State1, ELeft) ->
                           {ok, {op, Line, map_op(Op), ELeft}, State1}
                   end).

map_op('+') -> '+';
map_op('-') -> '-';
map_op('*') -> '*';
map_op('/') -> '/';
map_op('//') -> 'div';
map_op('%') -> 'rem';
map_op('|') -> 'bor';
map_op('&') -> 'band';
map_op('^') -> 'bxor';
map_op('>>') -> 'bsr';
map_op('<<') -> 'bsl';
map_op('~') -> 'bnot';
map_op('and') -> 'andalso';
map_op('or') -> 'orelse';
map_op('xor') -> 'xor';
map_op('!') -> '!';
map_op('not') -> 'not';
map_op('++') -> '++';
map_op('--') -> '--';
map_op('<') -> '<';
map_op('<=') -> '=<';
map_op('>') -> '>';
map_op('>=') -> '>=';
map_op('==') -> '=:=';
map_op('!=') -> '=/='.

with_converted(Nodes, State, Fun) ->
    FR = lists:foldl(fun (Node, {StateIn, Accum}) ->
                                 case convert(Node, StateIn) of
                                     {ok, R, StateOut} -> {StateOut, [R|Accum]};
                                     {error, Error, StateOut} ->
                                         {add_error(StateOut, Node, Error), Accum}
                                 end
                         end, {State, []}, Nodes),
    {State1, ENodes} = FR,
    NodesLen = length(Nodes),
    ENodesLen = length(ENodes),
    if NodesLen == ENodesLen -> apply(Fun, [State1|lists:reverse(ENodes)]);
       true -> {error, parsing_expr, State1}
    end.

add_export(Name, Arity, State=#{exported := Exported}) ->
    State#{exported => [{Name, Arity}|Exported]}.

list_to_cons_list(Line, Val, State) ->
    list_to_cons_list_r(Line, lists:reverse(Val), {nil, Line}, State).

list_to_cons_list_r(_Line, [], Cons, State) ->
    {Cons, State};

list_to_cons_list_r(Line, [H|T], Cons, State) ->
    case convert(H, State) of
        {ok, EH, State1} ->
            list_to_cons_list_r(Line, T, {cons, Line, EH, Cons}, State1);
        {error, Error, State1} ->
            list_to_cons_list_r(Line, T, Cons, add_error(State1, H, Error))
    end.

check_clauses([], _Name, _AllowElse) -> true;
check_clauses([?CL(?C([Name|_]))|T], Name, AllowElse) -> 
    check_clauses(T, Name, AllowElse);
check_clauses([?CL(?C(['else'], []))], _Name, AllowElse) -> 
    if AllowElse -> ok;
       true -> {error, else_not_allowed}
    end;
check_clauses([Other|_], _Name, _AllowElse) -> 
    {error, {bad_clause, Other}}.

convert_when_clauses(Line, 'when', Names, Args, Body, State) ->
    Cond = ?C(Line, Names, Args),
    case convert(Cond, State) of
        {ok, ECond, State1} ->
            case to_erl(Body, State1) of
                {EBody, State2} ->
                    R = {clause, Line, [], [[ECond]], EBody},
                    {ok, R, State2};
                Other -> Other
            end;
        Other -> Other
    end;
convert_when_clauses(Line, 'else', [], [], Body, State) ->
    io:format("~p else ~p~n", [Line, Body]),
    case to_erl(Body, State) of
        {EBody, State1} ->
            R = {clause, Line, [], [[{atom, Line, true}]], EBody},
            {ok, R, State1};
        Other -> Other
    end.

with_clauses([], State, _Fun, Accum) ->
    {lists:reverse(Accum), State};
with_clauses([Clause=?CL(?C(Line, [Type|Rest], Args), Body)|T], State, Fun, Accum) ->
    case Fun(Line, Type, Rest, Args, Body, State) of
        {ok, R, State1} ->
            with_clauses(T, State1, Fun, [R|Accum]);
        {error, Reason, State1} ->
            State2 = add_error(State1, Clause, Reason),
            with_clauses(T, State2, Fun, Accum)
    end.

