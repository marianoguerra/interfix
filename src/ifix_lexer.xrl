Definitions.

% numbers
Number      = [0-9]
Float       = [0-9]+\.[0-9]+([eE][-+]?[0-9]+)?

% delimiters and operators
Open        = \(
Close       = \)
OpenList    = \[
CloseList   = \]
OpenMap     = \{
CloseMap    = \}
Sep         = ,
SemiColon   = ;
Endls       = (\s|\t)*(\r?\n)
Whites      = \s+
Tabs        = \t+
Colon       = :
Split       = ::
Dot         = \.
Hash        = #

% string stuff
String      = "(\\\^.|\\.|[^\"])*"
BString     = '(\\\^.|\\.|[^\'])*'
AtomString  = `(\\\^.|\\.|[^\`])*`

% identifiers and atoms
Identifier  = [A-Z\_][a-zA-Z0-9\_]*
Atom        = ([a-z<=>\+\-\*\/\%\^\&\?\|\!\_][a-z<=>\+\-\*\/\%\^\&\?\|\!\_A-Z0-9]*)
Kw          = :[a-z<=>\+\-\*\/\%\^\&\?\|\!\_A-Z0-9]+

Rules.

% numbers
{Float}                  : make_token(float,   TokenLine, TokenChars, fun erlang:list_to_float/1).
{Number}+                : make_token(integer, TokenLine, TokenChars, fun erlang:list_to_integer/1).

% delimiters and operators
{Open}                   : make_token(open,        TokenLine, TokenChars).
{Close}                  : make_token(close,       TokenLine, TokenChars).
{Hash}{OpenList}         : make_token(open_tuple,  TokenLine, TokenChars).
{OpenList}               : make_token(open_list,   TokenLine, TokenChars).
{CloseList}              : make_token(close_list,  TokenLine, TokenChars).
{OpenMap}                : make_token(open_map,    TokenLine, TokenChars).
{CloseMap}               : make_token(close_map ,  TokenLine, TokenChars).

{Sep}                    : make_token(sep,         TokenLine, TokenChars).
{SemiColon}              : make_token(semicolon,   TokenLine, TokenChars).
{Send}                   : make_token(send_op,     TokenLine, TokenChars).
{Hash}                   : make_token(atom,        TokenLine, TokenChars).
{At}                     : make_token(at,          TokenLine, TokenChars).
{Split}                  : make_token(split,       TokenLine, TokenChars).
{Colon}                  : make_token(colon,       TokenLine, TokenChars).
{Dot}                    : make_token(dot,         TokenLine, TokenChars).
{Arrow}                  : make_token(arrow,       TokenLine, TokenChars).

% string stuff
{String}                 : build_string(string, TokenChars, TokenLine, TokenLen).
{BString}                : build_string(bstring, TokenChars, TokenLine, TokenLen).

% identifiers and atoms
{Identifier}             : make_token(var, TokenLine, TokenChars).
{Kw}                     : {token, atom(kw, tl(TokenChars), TokenLine)}.
{Atom}                   : {token, atom(atom, TokenChars, TokenLine)}.
{AtomString}             : build_atom_string(TokenChars, TokenLine, TokenLen).

% spaces, tabs and new lines
{Endls}                 : make_token(nl, TokenLine, endls(TokenChars)).
{Whites}                : skip_token.
{Tabs}                  : skip_token.

Erlang code.

make_token(Name, Line, Chars) when is_list(Chars) ->
    {token, {Name, Line, list_to_atom(Chars)}};
make_token(Name, Line, Chars) ->
    {token, {Name, Line, Chars}}.

make_token(Name, Line, Chars, Fun) ->
    {token, {Name, Line, Fun(Chars)}}.

endls(Chars) ->
    lists:filter(fun (C) -> C == $\n orelse C == $; end, Chars).

atom(Type, String, TokenLine) ->
    {Type, TokenLine, build_atom(String, TokenLine)}.

build_atom_string(Chars, Line, Len) ->
  String = unescape_string(lists:sublist(Chars, 2, Len - 2), Line),
  {token, {atom, Line, list_to_atom(String)}}.

build_string(Type, Chars, Line, Len) ->
  String = unescape_string(lists:sublist(Chars, 2, Len - 2), Line),
    {token, {Type, Line, String}}.

unescape_string(String, Line) -> unescape_string(String, Line, []).

unescape_string([], _Line, Output) ->
  lists:reverse(Output);
unescape_string([$\\, Escaped | Rest], Line, Output) ->
  Char = map_escaped_char(Escaped, Line),
  unescape_string(Rest, Line, [Char|Output]);
unescape_string([Char|Rest], Line, Output) ->
  unescape_string(Rest, Line, [Char|Output]).

map_escaped_char(Escaped, Line) ->
  case Escaped of
    $\\ -> $\\;
    $/ -> $/;
    $\" -> $\";
    $\' -> $\';
    $\( -> $(;
    $b -> $\b;
    $d -> $\d;
    $e -> $\e;
    $f -> $\f;
    $n -> $\n;
    $r -> $\r;
    $s -> $\s;
    $t -> $\t;
    $v -> $\v;
    _ -> throw({error, {Line, fn_lexer, ["unrecognized escape sequence: ", [$\\, Escaped]]}})
  end.

build_atom(Atom, _Line) -> list_to_atom(Atom).
