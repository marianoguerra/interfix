-module('interfix').

%% API exports
-export([to_lex/1, to_ast/1, to_erl_ast/1, to_erl/1, to_mod/1, to_code/1,
         to_code/2, compile/2, compile/3]).
-export([str_to_lex/1, str_to_ast/1, str_to_erl_ast/1]).

%% CLI exports
-export([main/1]).

%%====================================================================
%% API functions
%%====================================================================
%%
read_file(Path) ->
    case file:read_file(Path) of
        {ok, Content} -> {ok, unicode:characters_to_list(Content, utf8)};
        Other -> Other
    end.

with_file_content(Path, Fn) ->
    case read_file(Path) of
        {ok, Content} -> Fn(Content);
        Other -> Other
    end.

to_lex(Path) -> with_file_content(Path, fun str_to_lex/1).
to_ast(Path) -> with_file_content(Path, fun str_to_ast/1).
to_erl_ast(Path) -> with_file_content(Path, fun str_to_erl_ast/1).  
to_erl(Path) ->
    case to_mod(Path) of
        {ok, Mod} -> erl_prettypr:format(erl_syntax:form_list(Mod));
        Other -> Other
    end.
to_mod(Path) ->
    case to_erl_ast(Path) of
        {Ast, State=#{exported := Exports}} ->
            ModAtomName = get_module_name(Path),
            ToMod = fun () ->
                            ModAttr = {attribute, 1, module, ModAtomName},
                            FileAttr = {attribute, 1, file, {Path, 1}},
                            ExportAttr = {attribute, 1, export, lists:reverse(Exports)},
                            {ok, [FileAttr, ModAttr, ExportAttr|Ast]}
                    end,
            format_errors_or(ModAtomName, State, ToMod);
        Other -> Other
    end.

to_code(Path) ->
    to_code(Path, []).

to_code(Path, Opts) ->
    case to_mod(Path) of
        {ok, Ast} ->
            case compile:forms(Ast, [return, strong_validation|Opts]) of
                {error, _Errors, _Warnings}=Error -> Error;
                error -> {error, [{error, compile_forms_error}], []};
                _ ->
                    case compile:forms(Ast, Opts) of
                        {ok, ModuleName, Code} -> {ok, ModuleName, Code, []};
                        {ok, _ModuleName, _Code, _Warnings}=Res -> Res;
                        error -> {error, [{error, compile_forms_error}], []};
                        {error, _Errors, _Warnings}=Error -> Error
                    end
            end;
        Other -> Other
    end.

compile(Path, DestPath) ->
    compile(Path, DestPath, []).

compile(Path, DestPath, Opts) ->
    case to_code(Path, Opts) of
        {ok, ModuleName, Code, Warnings} ->
            BeamPath = filename:join(DestPath, get_module_beam_name(Path)),
            case bin_to_file(Code, BeamPath) of
                error -> {error, [{file_write_error, BeamPath}], []};
                ok -> {ok, [{warnings, Warnings}, {module_name, ModuleName}]}
            end;
        Other -> Other
    end.

bin_to_file(Bin, Path) ->
    to_file(Bin, Path, [binary, write]).

to_file(Data, Path, Mode) ->
    case file:open(Path, Mode) of
        {ok, Device} ->
            file:write(Device, Data),
            file:close(Device),
            ok;
        Error -> Error
    end.

get_module_beam_name(Path) ->
    ModuleNameStr = get_module_str_name(Path),
    string:concat(ModuleNameStr, ".beam").

get_module_str_name(Path) ->
    BaseName = filename:basename(Path),
    filename:rootname(BaseName).

get_module_name(Path) ->
    list_to_atom(get_module_str_name(Path)).
format_errors_or(_Module, #{errors:=[]}, Fn) -> Fn();
format_errors_or(Module, #{errors:=Errors}, _Fn) ->
    ErrorsFirstToLast = lists:reverse(Errors),
    lists:foreach(fun (#{error := Error, code := Ast}) ->
                          Line = line(Ast),
                          io:format("error:~p:~p: ~p~n~p~n~n", [Module, Line, Error, Ast])
                  end, ErrorsFirstToLast),
    {error, compile_errors}.

str_to_lex(String) ->
    case ifix_lexer:string(String) of
        {ok, Tokens, Endline} ->
            CleanTokens = clean_tokens(Tokens),
            {ok, CleanTokens, Endline};
        {eof, Endline} -> {error, {Endline, fn_lexer, {eof, Endline}}};
        {error, Error} -> {error, Error};
        {error, Error, _} -> {error, Error}
    end.

str_to_ast(Str) ->
    case str_to_lex(Str) of
        {ok, Tokens, _NewLine} -> ifix_parser:parse(Tokens);
        Other -> Other
    end.

str_to_erl_ast(Str) ->
    case str_to_ast(Str) of
        {ok, Ast} -> ifix_to_erl:to_erl(Ast);
        Other -> Other
    end.

clean_tokens(Tokens) -> clean_tokens(Tokens, []).

clean_tokens([], Accum) -> lists:reverse(Accum);
% remove newline after colon, semicolon
clean_tokens([{colon, _, _}=H, {nl, _, _}|T], Accum) -> clean_tokens([H|T], Accum);
clean_tokens([{semicolon, _, _}=H, {nl, _, _}|T], Accum) -> clean_tokens([H|T], Accum);
% remove duplicated endlines
clean_tokens([{nl, _, _}, {nl, _, _}=H|T], Accum) -> clean_tokens([H|T], Accum);
% remove newline before dots
clean_tokens([{nl, _, _}, V={dot, _, _}|T], Accum) ->
    clean_tokens([V|T], Accum);
% remove newline before splits
clean_tokens([{nl, _, _}, V={split, _, _}|T], Accum) ->
    clean_tokens([V|T], Accum);
% remove last endline
clean_tokens([{nl, _, _}], Accum) -> clean_tokens([], Accum);
clean_tokens([H|T], Accum) -> clean_tokens(T, [H|Accum]).

normalize_error({error, Reason}) -> io_lib:format("~p", [Reason]).

print({ok, Data}) ->
    print(Data);
print({error, _}=Error) ->
    Reason = normalize_error(Error),
    io:format("error:~s~n", [Reason]);
print(Data) ->
    try io:format("~s~n", [Data]) catch
        _:_ -> io:format("~p~n", [Data])
    end.

%% escript Entry point
main(["lex", Path]) ->
    case to_lex(Path) of
        {ok, Data, _EndLine} -> print(Data);
        Other -> print(Other)
    end;
main(["ast", Path]) -> print(to_ast(Path));
main(["erl", Path]) -> print(to_erl(Path));
main(["mod", Path]) -> print(to_mod(Path));
main(["erlast", Path]) -> print(to_erl_ast(Path));
main(["beam", Path, DestPath]) -> print(compile(Path, DestPath));
main(Args) ->
    io:format("Unknown Command: ~p~n", [Args]),
    erlang:halt(0).

%%====================================================================
%% Internal functions
%%====================================================================

line(T) when is_tuple(T) -> element(2, T);
line([H|_]) -> line(H).
