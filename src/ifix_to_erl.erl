-module(ifix_to_erl).
-export([to_erl/1]).

-include("../include/ifix.hrl").

-record(call, {line=2, names=[], args=[]}).
-define(EMPTY_CALL, #call{names=[], args=[]}).
-define(Call(Names, Args), #call{names=Names, args=Args}).
-define(Call(Line, Names, Args), #call{line=Line, names=Names, args=Args}).

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

reverse_call(#call{names=Names, args=Args}) ->
    #call{names=lists:reverse(Names), args=lists:reverse(Args)}.

split_guards(Names, Args) ->
    split_guards(Names, Args, ?EMPTY_CALL, [], ?EMPTY_CALL, names).

split_guards([], [], FCall, Guards, ?EMPTY_CALL, _) ->
    {reverse_call(FCall), lists:reverse(lists:map(fun reverse_call/1, Guards))};
split_guards(Names=[], Args=[], FCall, Guards, CurGuard, State) ->
    split_guards(Names, Args, FCall, [CurGuard|Guards], ?EMPTY_CALL, State);


split_guards([{split, _, _}|T], Args, FCall, Guards, CurGuard=?EMPTY_CALL, names) ->
    split_guards(T, Args, FCall, Guards, CurGuard, guards);

split_guards([{split, _, _}|T], Args, FCall, Guards, CurGuard=?EMPTY_CALL, State=guards) ->
    split_guards(T, Args, FCall, Guards, CurGuard, State);

split_guards([{split, _, _}|T], Args, FCall, Guards, CurGuard, State=guards) ->
    split_guards(T, Args, FCall, [CurGuard|Guards], ?EMPTY_CALL, State);


split_guards([Name='_'|T], [Arg|Args], ?Call(CNames, CArgs), Guards,
             CurGuard=?EMPTY_CALL, State=names) ->
    NewCall = ?Call([Name|CNames], [Arg|CArgs]),
    split_guards(T, Args, NewCall, Guards, CurGuard, State);

split_guards([Name='_'|T], [Arg|Args], FCall, Guards,
             ?Call(CNames, CArgs), State=guards) ->
    NewGuard = ?Call([Name|CNames], [Arg|CArgs]),
    split_guards(T, Args, FCall, Guards, NewGuard, State);

split_guards([Name|T], Args, ?Call(CNames, CArgs), Guards,
             CurGuard=?EMPTY_CALL, State=names) ->
    NewCall = ?Call([Name|CNames], CArgs),
    split_guards(T, Args, NewCall, Guards, CurGuard, State);

split_guards([Name|T], Args, FCall, Guards,
             ?Call(CNames, CArgs), State=guards) ->
    NewGuard = ?Call([Name|CNames], CArgs),
    split_guards(T, Args, FCall, Guards, NewGuard, State).

guards_to_erl(Guards, State) -> guards_to_erl(Guards, State, []).

guards_to_erl([], State, Accum) ->
    {lists:reverse(Accum), State};

guards_to_erl([Guard=?Call(['when'|Names], Args)|T], State, Accum) ->
    case convert(?C(2, Names, Args), State) of
        {ok, R, State1} ->
            guards_to_erl(T, State1, [R|Accum]);
        {error, Error, State1} ->
            State2 = add_error(State1, Guard, Error),
            guards_to_erl(T, State2, Accum)
    end;
guards_to_erl([Other|T], State, Accum) ->
    State1 = add_error(State, Other, invalid_guard),
    guards_to_erl(T, State1, Accum).


convert_fn_clauses(Clauses=[?CL(?C([_Fn|FNames], Args))|_], State) ->
    {#call{names=FName, args=FArgs}, _Guards} = split_guards(FNames, Args),
    Arity = length(FArgs),
    convert_fn_clauses(Clauses, FName, Arity, [], State).

convert_fn_clauses([], Name, Arity, Accum, State) ->
    {ok, to_name(Name), Arity, lists:reverse(Accum), State};
convert_fn_clauses([?CL(Line, ?C([_Fn|FNames], CArgs), Body)|T], Name, Arity, Accum, State) ->
    {#call{names=FName, args=FArgs}, Guards} = split_guards(FNames, CArgs),
    CurArity = length(FArgs),
    if CurArity /= Arity -> {error, head_arity_mismatch, State};
       FName /= Name -> {error, head_name_mismatch, State};
       true ->
           {EBody, State1} = to_erl(Body, [], State),
           {EArgs, State2} = to_erl(FArgs, [], State1),
           {EGuards, State3} = guards_to_erl(Guards, State2),
           EClause = {clause, Line, EArgs, EGuards, EBody},
           convert_fn_clauses(T, Name, Arity, [EClause|Accum], State3)
    end.

convert(?C(Line, [fn, ref, FName, '_'], [{integer, _, Arity}]), State)
         when FName /= '_' ->
    {ok, {'fun', Line, {function, FName, Arity}}, State};

convert(?C(Line, [fn, ref, ModName, FName, '_'], [{integer, _, Arity}]), State)
         when ModName /= '_', FName /= '_' ->
    {ok, {'fun', Line, {function, ModName, FName, Arity}}, State};

convert(?C(Line, [fn, ref|Names], _), State) ->
    Arity = length(lists:filter(fun('_') -> true; (_) -> false end, Names)),
    case to_name(Names) of
        [FName] ->
            {ok, {'fun', Line, {function, FName, Arity}}, State};
        [ModName, FName] ->
            {ok, {'fun', Line, {function, ModName, FName, Arity}}, State}
    end;

convert(?CB(Line, Clauses=[?CL(?C([Fn|_FName]))|_]), State=#{level := Level})
        when Fn == 'fn'; Fn == 'fn+' ->
    if Level == 0 ->
           Exported = Fn == 'fn+',
           case convert_fn_clauses(Clauses, State#{level => Level + 1}) of
               {ok, [Name], Arity, EClauses, State1} ->
                   State2 = if Exported -> add_export(Name, Arity, State1);
                               true -> State1
                            end,
                   {ok, {function, Line, Name, Arity, EClauses}, State2#{level => Level}};
               {error, Error, State1} ->
                   {error, Error, State1}
           end;
       Fn == 'fn' ->
           case {check_clauses_shape(Clauses, fun check_lambda_shape/2),
                 check_clauses_same_arity(Clauses)} of
               {ok, ok} ->
                   {EClauses, State1} = with_clauses(Clauses, State#{level => Level + 1},
                                                     fun convert_lambda_clauses/6, []),
                   R = {'fun', Line, {clauses, EClauses}},
                   {ok, R, State1#{level => Level}};
               {{error, Reason}, _} ->
                   {error, {bad_expr, 'fun', Reason}, State};
               {_, {error, Reason}} ->
                   {error, {bad_expr, 'fun', Reason}, State}
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

convert(?CB(Line, Clauses=[?CCS(['if', '_', is, '_'|_])|_]), State) ->
    case check_clauses_shape(Clauses, fun check_case_shape/2) of
        ok ->
            {MEClauses, State1} = with_clauses(Clauses, State,
                                       fun convert_case_clauses/6, []),
            [[EMatch, EFirstClause]|ERestClauses] = MEClauses,
            EClauses = [EFirstClause|ERestClauses],
            R = {'case', Line, EMatch, EClauses},
            {ok, R, State1};
        {error, Reason} ->
            {error, {bad_expr, 'case', Reason}, State}
    end;

convert(?CB(Line, Clauses=[?CCS(['try'])|_]), State) ->
    case check_clauses_shape(Clauses, fun check_try_shape/2) of
        ok ->
            Vs = with_clauses(Clauses, State, fun convert_try_clauses/6, []),
            case Vs of
                {[{'finally', _, EFBody}, {'try', _, ETBody}|EClauses], State1} ->
                    R = {'try', Line, ETBody, [], EClauses, EFBody},
                    {ok, R, State1};
                {[{'try', _, ETBody}|EClauses], State1} ->
                    R = {'try', Line, ETBody, [], EClauses, []},
                    {ok, R, State1}
            end;
        {error, Reason} ->
            {error, {bad_expr, 'try', Reason}, State}
    end;
convert(?CB(Line, Clauses=[?CCS([on, message, '_'|_])|_]), State) ->
    case check_clauses_shape(Clauses, fun check_recv_shape/2) of 
        ok ->
            Vs = with_clauses(Clauses, State, fun convert_recv_clauses/6, []),
            case Vs of
                {[{'after', _,  ETimeout, EAfterBody}|EClauses], State1} ->
                    R = {'receive', Line, EClauses, ETimeout, EAfterBody},
                    {ok, R, State1};
                {EClauses, State1} ->
                    R = {'receive', Line, EClauses},
                    {ok, R, State1}
            end;
        {error, Reason} ->
            {error, {bad_expr, 'receive', Reason}, State}
    end;

convert(?CB([?CL({call, _, {['do'], _}}, Body)]), State) ->
    with_converted(Body, State,
                   fun (State1, EBody) ->
                           {ok, to_block(EBody), State1}
                   end);

convert(?CB(Line, [?CL(?C(GNames=['for', '_', in, '_'|_], GArgs), Body)]), State) ->
    {FirstGen, Guards} = split_guards(tl(GNames), GArgs),
    compile_lc(lc, Line, [FirstGen|Guards], Body, State, []);

convert(?CB(Line, [?CL(?C(GNames=['for', '_', in, bin, '_'|_], GArgs), Body)]), State) ->
    {FirstGen, Guards} = split_guards(tl(GNames), GArgs),
    compile_lc(lc, Line, [FirstGen|Guards], Body, State, []);

convert(?CB(Line, [?CL(?C(GNames=[bin, 'for', '_', in, '_'|_], GArgs), Body)]), State) ->
    {FirstGen, Guards} = split_guards(tl(tl(GNames)), GArgs),
    compile_lc(bc, Line, [FirstGen|Guards], Body, State, []);

convert(?CB(Line, [?CL(?C(GNames=[bin, 'for', '_', in, bin, '_'|_], GArgs), Body)]), State) ->
    {FirstGen, Guards} = split_guards(tl(tl(GNames)), GArgs),
    compile_lc(bc, Line, [FirstGen|Guards], Body, State, []);

convert(V={var, _, _}, State) -> {ok, V, State};
convert(V={integer, _, _}, State) -> {ok, V, State};
convert(V={float, _, _}, State) -> {ok, V, State};
convert(V={string, _, _}, State) -> {ok, V, State};
convert({bstring, Line, Val}, State) ->
    R = {bin, Line, [{bin_element, Line, {string, Line, Val}, default, default}]},
   {ok, R, State};
convert({kw, Line, Val}, State) -> {ok, {atom, Line, Val}, State};
convert({list, Line, Val}, State) ->
    {R, State1} = list_to_cons_list(Line, Val, State),
    {ok, R, State1};
convert({tuple, Line, Val}, State) ->
    {EVal, State1} = to_erl(Val, [], State),
    {ok, {tuple, Line, EVal}, State1};

convert({map, Line, _Items}=Node, State) ->
    {Pairs, State1} = group_map_items(Node, State),
    {EPairs, State2} = convert_map_items(Pairs, State1),
    R = {map, Line, EPairs},
    {ok, R, State2};

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

convert(?C(Line, [RecName, '#', '_'], [Map={map, _, _}]), State) ->
    {RFields, State1} = convert_record_fields(Map, State),
    R = {record, Line, RecName, RFields},
    {ok, R, State1};

convert(?C(Line, [RecName, '#', '_', '_'], [Var={var, _, _}, Map={map, _, _}]), State) ->
    {RFields, State1} = convert_record_fields(Map, State),
    R = {record, Line, Var, RecName, RFields},
    {ok, R, State1};

convert(?C(Line, [RecName, '#', FieldName, '_'], [Var={var, _, _}]), State) ->
    R = {record_field, Line, Var, RecName, {atom, Line, FieldName}},
    {ok, R, State};

convert(?C(Line, [RecName, '#', FieldName], []), State) ->
    R = {record_index, Line, RecName, {atom, Line, FieldName}},
    {ok, R, State};


convert(?Op(Line, '#', Var={var, _, _}, Map={map, _, _}), State) ->
    with_converted([Var, Map], State,
                   fun (State1, [EVar, {map, _, EMapItems}]) ->
                           R = {map, Line, EVar, EMapItems},
                           {ok, R, State1}
                   end);

convert(?C(Line, ['_', is|RNames], [Left|RVals]), State) ->
    with_converted([Left, ?C(Line, RNames, RVals)], State,
                   fun (State1, [ELeft, ERight]) ->
                           {ok, {match, Line, ELeft, ERight}, State1}
                   end);

convert(?C(Line, [cons, '_', '_'], [H, T]), State) ->
    with_converted([H, T], State,
                   fun (State1, [EH, ET]) ->
                           {ok, {cons, Line, EH, ET}, State1}
                   end);
convert(?C(['_'], [V]), State) ->
    convert(V, State);

convert(Call=?C([First|_]), State) when First /= '_' ->
    convert_call(Call, State);

convert(?C(Line, ['_', {split, _, _}, '_'|_Names],
         [?V(VMLine, ModName), ?V(VFLine, FunName)|Args]), State) ->
    {EArgs, State1} = to_erl(Args, [], State),
    {ok, {call, Line,
          {remote, Line, {var, VMLine, ModName}, {var, VFLine, FunName}},
          EArgs}, State1};

convert(?C(Line, _Names, [?V(VLine, FunName)|Args]), State) ->
    {EArgs, State1} = to_erl(Args, [], State),
    {ok, {call, Line, {var, VLine, FunName}, EArgs}, State1};

convert(Call=?C(Line, [_|Names], [?KW(VLine, Name)|Args]), State) ->
    case all_values(Names) of
        true ->
            {EArgs, State1} = to_erl(Args, [], State),
            {ok, {tuple, Line, [{atom, VLine, Name}|EArgs]}, State1};
        false ->
            convert_call(Call, State)
    end;

convert(_, State) ->
    {error, unknown_node, State}.

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
                   fun (State1, [ELeft, ERight]) ->
                           {ok, {op, Line, map_op(Op), ELeft, ERight}, State1}
                   end).

op(Line, Op, Left, State) ->
    with_converted([Left], State,
                   fun (State1, [ELeft]) ->
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

convert_nodes(Nodes, State) ->
    R = lists:foldl(fun (Node, {Status, Accum, StateIn}) ->
                            case convert(Node, StateIn) of
                                {ok, R, StateOut} ->
                                    {Status, [R|Accum], StateOut};
                                {error, Error, StateOut} ->
                                    {error, Accum, add_error(StateOut, Node, Error)}
                            end
                    end, {ok, [], State}, Nodes),
    {Status, ENodes, State1} = R,
    {Status, lists:reverse(ENodes), State1}.

with_converted(Nodes, State, Fun) ->
    case convert_nodes(Nodes, State) of
        {ok, ENodes, State1} ->
            Fun(State1, ENodes);
        {error, _ENodes, State1} ->
            {error, parsing_expr, State1}
    end.

with_converted(Nodes, Body, State, Fun) ->
    case convert_nodes(Nodes, State) of
        {ok, ENodes, State1} ->
            case convert_nodes(Body, State1) of
                {ok, EBody, State2} ->
                    Fun(State2, ENodes, EBody);
                {error, _ENodes, State2} ->
                    {error, parsing_body_expr, State2}
            end;
        {error, _ENodes, State1} ->
            {error, parsing_expr, State1}
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
    case to_erl(Body, State) of
        {EBody, State1} ->
            R = {clause, Line, [], [[{atom, Line, true}]], EBody},
            {ok, R, State1};
        Other -> Other
    end.

convert_case_clauses(Line, 'if', ['_', is, '_'|GNames], [Expr, Match|GArgs], Body, State) ->
    {_, Guards} = split_guards(GNames, GArgs),
    {EGuards, State1} = guards_to_erl(Guards, State),
    with_converted([Expr, Match], Body, State1,
                   fun (State2, [EExpr, EMatch], EBody) ->
                           R = [EExpr, {clause, Line, [EMatch], EGuards, EBody}],
                           {ok, R, State2}
                   end);
convert_case_clauses(Line, 'if', [it, is, '_'|GNames], [Match|GArgs], Body, State) ->
    {_, Guards} = split_guards(GNames, GArgs),
    {EGuards, State1} = guards_to_erl(Guards, State),
    with_converted([Match], Body, State1,
                   fun (State2, [EMatch], EBody) ->
                           R = {clause, Line, [EMatch], EGuards, EBody},
                           {ok, R, State2}
                   end);
convert_case_clauses(Line, 'else', [], [], Body, State) ->
    case to_erl(Body, State) of
        {EBody, State1} ->
            R = {clause, Line, [{var, Line, '_'}], [], EBody},
            {ok, R, State1};
        Other -> Other
    end.

convert_recv_clauses(Line, 'on', [message, '_'|GNames], [Match|GArgs], Body, State) ->
    {_, Guards} = split_guards(GNames, GArgs),
    {EGuards, State1} = guards_to_erl(Guards, State),
    with_converted([Match], Body, State1,
                   fun (State2, [EMatch], EBody) ->
                           R = {clause, Line, [EMatch], EGuards, EBody},
                           {ok, R, State2}
                   end);
convert_recv_clauses(Line, 'after', ['_', milliseconds], [AfterExpr], Body, State) ->
    with_converted([AfterExpr], Body, State,
                   fun (State1, [EAfter], EBody) ->
                           R = {'after', Line, EAfter, EBody},
                           {prepend, R, State1}
                   end).

convert_try_clauses(Line, 'try', [], [], Body, State) ->
    with_converted(Body, State,
                   fun (State1, EBody) ->
                           {ok, {'try', Line, EBody}, State1}
                   end);

convert_try_clauses(Line, 'catch', ['_', '_'|GNames], [Type, Catch|GArgs], Body, State) ->
    {_, Guards} = split_guards(GNames, GArgs),
    {EGuards, State1} = guards_to_erl(Guards, State),
    with_converted([Type, Catch], Body, State1,
                   fun (State2, [EType, ECatch], EBody) ->
                           R = {clause, Line,
                                [{tuple, Line, [EType, ECatch,
                                                {var, Line, '_'}]}],
                                EGuards, EBody},
                           {ok, R, State2}
                   end);

convert_try_clauses(Line, 'catch', [Type, '_'|GNames], [Catch|GArgs], Body, State) ->
    {_, Guards} = split_guards(GNames, GArgs),
    {EGuards, State1} = guards_to_erl(Guards, State),
    with_converted([Catch], Body, State1,
                   fun (State2, [ECatch], EBody) ->
                           R = {clause, Line,
                                [{tuple, Line, [{atom, Line, Type}, ECatch,
                                                {var, Line, '_'}]}],
                                EGuards, EBody},
                           {ok, R, State2}
                   end);

convert_try_clauses(Line, 'always', [], [], Body, State) ->
    with_converted(Body, State,
                   fun (State1, EBody) ->
                           {prepend, {'finally', Line, EBody}, State1}
                   end).

convert_lambda_clauses(Line, 'fn', FNames, Args, Body, State) ->
    {#call{args=FArgs}, Guards} = split_guards(FNames, Args),
    with_converted(FArgs, Body, State,
                   fun (State1, EArgs, EBody) ->
                           {EGuards, State2} = guards_to_erl(Guards, State1),
                           {ok, {clause, Line, EArgs, EGuards, EBody}, State2}
                   end).

with_clauses([], State, _Fun, Accum) ->
    {lists:reverse(Accum), State};
with_clauses([Clause=?CL(?C(Line, [Type|Rest], Args), Body)|T], State, Fun, Accum) ->
    case Fun(Line, Type, Rest, Args, Body, State) of
        {ok, R, State1} ->
            with_clauses(T, State1, Fun, [R|Accum]);
        {prepend, R, State1} ->
            with_clauses(T, State1, Fun, Accum ++ [R]);
        {error, Reason, State1} ->
            State2 = add_error(State1, Clause, Reason),
            with_clauses(T, State2, Fun, Accum)
    end.

check_clauses_shape(Clauses, Fun) ->
    check_clauses_shape(Clauses, Fun, first).

check_clauses_shape([], _Fun, _Pos) -> ok;
check_clauses_shape([Clause|Clauses], Fun, Pos) ->
    case Fun(Clause, Pos) of
        ok -> check_clauses_shape(Clauses, Fun, next_pos(Pos, Clauses));
        {error, _Reason}=Error -> Error
    end.

next_pos(first, []) -> 'end'; % shouldn't be used
next_pos(first, [_]) -> last;
next_pos(first, _) -> middle;
next_pos(middle, []) -> 'end'; % shouldn't be used
next_pos(middle, [_]) -> last;
next_pos(middle, _) -> middle;
next_pos(last, _) -> 'end'. % shouldn't be used

check_try_shape(?CCS(['try']), first) -> ok;
check_try_shape(?CCS(['catch', error, '_']), middle) -> ok;
check_try_shape(?CCS(['catch', error, '_', {split, _, _}|_]), middle) -> ok;
check_try_shape(?CCS(['catch', throw, '_']), middle) -> ok;
check_try_shape(?CCS(['catch', throw, '_', {split, _, _}|_]), middle) -> ok;
check_try_shape(?CCS(['catch', exit, '_']), middle) -> ok;
check_try_shape(?CCS(['catch', exit, '_', {split, _, _}|_]), middle) -> ok;
check_try_shape(?CCS(['catch', '_', '_']), middle) -> ok;
check_try_shape(?CCS(['catch', '_', '_', {split, _, _}|_]), middle) -> ok;

check_try_shape(?CCS(['catch', error, '_']), last) -> ok;
check_try_shape(?CCS(['catch', error, '_', {split, _, _}|_]), last) -> ok;
check_try_shape(?CCS(['catch', throw, '_']), last) -> ok;
check_try_shape(?CCS(['catch', throw, '_', {split, _, _}|_]), last) -> ok;
check_try_shape(?CCS(['catch', exit, '_']), last) -> ok;
check_try_shape(?CCS(['catch', exit, '_', {split, _, _}|_]), last) -> ok;
check_try_shape(?CCS(['catch', '_', '_']), last) -> ok;
check_try_shape(?CCS(['catch', '_', '_', {split, _, _}|_]), last) -> ok;

check_try_shape(?CCS(['always']), last) -> ok;
check_try_shape(?CCS(Other), Pos) -> {error, {bad_try_clause, Pos, Other}}.


check_case_shape(?CCS(['if', '_', is, '_']), first) -> ok;
check_case_shape(?CCS(['if', '_', is, '_', {split, _, _}|_]), first) -> ok;
check_case_shape(?CCS(['if', it, is, '_']), middle) -> ok;
check_case_shape(?CCS(['if', it, is, '_', {split, _, _}|_]), middle) -> ok;
check_case_shape(?CCS(['if', it, is, '_']), last) -> ok;
check_case_shape(?CCS(['if', it, is, '_', {split, _, _}|_]), last) -> ok;
check_case_shape(?CCS([else]), last) -> ok;
check_case_shape(?CCS(Other), Pos) -> {error, {bad_case_clause, Pos, Other}}.

check_recv_shape(?CCS([on, message, '_']), first) -> ok;
check_recv_shape(?CCS([on, message, '_', {split, _, _}|_]), first) -> ok;
check_recv_shape(?CCS([on, message, '_']), middle) -> ok;
check_recv_shape(?CCS([on, message, '_', {split, _, _}|_]), middle) -> ok;
check_recv_shape(?CCS([on, message, '_']), last) -> ok;
check_recv_shape(?CCS([on, message, '_', {split, _, _}|_]), last) -> ok;
check_recv_shape(?CCS(['after', '_', milliseconds]), last) -> ok;
check_recv_shape(?CCS(Other), Pos) -> {error, {bad_receive_clause, Pos, Other}}.

check_lambda_shape(?CCS(['fn'|Names]), _) ->
    case lists:all(fun ('_') -> true; (_) -> false end, drop_guards(Names)) of
        true -> ok;
        false -> {error, {bad_lambda_clause, {invalid_arguments, Names}}}
    end.

check_clauses_same_arity([?CCS(['fn'|FNames])|Clauses]) ->
    Arity = length(drop_guards(FNames)),
    lists:foldl(fun (?CCS(['fn'|Names]), Status) ->
                        CurArity = length(drop_guards(Names)),
                        if CurArity =/= Arity -> {error, head_mismatch};
                           true  -> Status
                        end
                end, ok, Clauses).

all_values([]) -> true;
all_values(['_'|T]) -> all_values(T);
all_values(_) -> false.

convert_call(?C(Line, Names, Args), State) ->
    Name = to_call_name(Line, Names),
    {EArgs, State1} = to_erl(Args, [], State),
    {ok, {call, Line, Name, EArgs}, State1}.

drop_guards(Names) -> drop_guards(Names, []).

drop_guards([], Accum) -> lists:reverse(Accum);
drop_guards([{split, _, _}|_], Accum) -> lists:reverse(Accum);
drop_guards([H|T], Accum) -> drop_guards(T, [H|Accum]).

convert_map_items(Pairs, State) -> convert_map_items(Pairs, State, []).

convert_map_items([], State, Accum) ->
    {lists:reverse(Accum), State};
convert_map_items([{?C(KLine, ['_', '='], [Key]), Val}|T], State, Accum) ->
    Fun = fun (State1, [EKey, EVal]) ->
                  R = {map_field_exact, KLine, EKey, EVal},
                  {ok, R, State1}
          end,
    convert_map_pair(Key, Val, State, Accum, T, Fun);

convert_map_items([{Key, Val}|T], State, Accum) ->
    Fun = fun (State1, [EKey, EVal]) ->
                  KLine = element(2, EKey),
                  R = {map_field_assoc, KLine, EKey, EVal},
                  {ok, R, State1}
          end,
    convert_map_pair(Key, Val, State, Accum, T, Fun).


convert_map_pair(Key, Val, State, Accum, T, Fun) ->
    case with_converted([Key, Val], State, Fun) of
        {ok, R, State1} ->
            convert_map_items(T, State1, [R|Accum]);
        {error, _, State1} ->
            convert_map_items(T, State1, Accum)
    end.

convert_record_fields(Map, State) ->
    {Pairs, State1} = group_map_items(Map, State),
    convert_record_fields(Pairs, State1, []).

convert_record_fields([], State, Accum) -> {lists:reverse(Accum), State};
convert_record_fields([{{atom, KLine, K}, Val}|T], State, Accum) ->
    case convert(Val, State) of
        {ok, EVal, State1} ->
            EField = {record_field, KLine, {atom, KLine, K}, EVal},
            convert_record_fields(T, State1, [EField|Accum]);
        {error, Reason, State1} ->
            State2 = add_error(State1, Val, {bad_record_value, Reason}),
            convert_record_fields(T, State2, Accum)
    end;

convert_record_fields([{Other, _Val}|T], State, Accum) ->
    State1 = add_error(State, Other, bad_record_key),
    convert_record_fields(T, State1, Accum).

group_map_items({map, _Line, Items}=Node, State) ->
    State1 = if length(Items) rem 2 /= 0 ->
                    add_error(State, Node, odd_number_of_map_items);
                true -> State
             end,

    {PairsR, _} = lists:foldl(fun (Item, {Accum, nil}) ->
                                      {Accum, Item};
                                  (Item, {Accum, First}) ->
                                      {[{First, Item}|Accum], nil}
                              end, {[], nil}, Items),

    Pairs = lists:reverse(PairsR),
    {Pairs, State1}.

compile_lc(Type, Line, [], Body, State, Accum) ->
    EGFs = lists:reverse(Accum), 
    case to_erl(Body, State) of
        {EBody, State1} ->
            R = {Type, Line, to_block(EBody), EGFs},
            {ok, R, State1};
        Other -> Other
    end;
compile_lc(Type, Line, [?Call(GLine, ['_', in, '_'], [Val, Seq])|T], Body, State, Accum) ->
    with_converted([Val, Seq], State,
                   fun (State1, [EVal, ESeq]) ->
                           R = {generate, GLine, EVal, ESeq},
                           compile_lc(Type, Line, T, Body, State1, [R|Accum])
                   end);
compile_lc(Type, Line, [?Call(GLine, ['_', in, bin, '_'], [Val, Seq])|T], Body, State, Accum) ->
    with_converted([Val, Seq], State,
                   fun (State1, [EVal, ESeq]) ->
                           R = {b_generate, GLine, EVal, ESeq},
                           compile_lc(Type, Line, T, Body, State1, [R|Accum])
                   end);
compile_lc(Type, Line, [?Call(GLine, ['when'|Names], Args)|T], Body, State, Accum) ->
    Call = ?C(GLine, Names, Args),
    case convert(Call, State) of
        {ok, R, State1} ->
            compile_lc(Type, Line, T, Body, State1, [R|Accum]);
        {error, Error, State1} ->
            State2 = add_error(State1, Call, Error),
            compile_lc(Type, Line, T, Body, State2, Accum)
    end;
compile_lc(Type, Line, [Other|T], Body, State, Accum) ->
    State1 = add_error(State, Other, bad_for_expr),
    compile_lc(Type, Line, T, Body, State1, Accum).

to_block([Expr]) ->
    Expr;
to_block(EBody=[H|_]) ->
    Line = element(2, H),
    {'block', Line, EBody}.
