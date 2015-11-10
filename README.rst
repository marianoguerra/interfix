interfix
========

An experimential programming language for the Erlang VM.

prefix? infix? postfix? none, interfix
underscores? camel case? pascal case? none, interfix

interfix is an experiment to break with some common "expectations" in
programming languages and provide a simpler yet expressive programming language
that is actually useful.

First some working examples:

.. code-block:: ruby

    fn main:
        when A < 10:
            do something with A
            something else;
        when A < 20:
            log warning "A is ~p" [A];
        else:
            log warn "wat".
    .

.. code-block:: ruby

    fn divide A by 0:
        #[:error :division_by_zero];
    fn divide A by B:
        #[:ok (A / B)].

    fn format Str with Args:
        io :: format Str Args.

    fn+ export this one 0:
        0;
    fn export this one A:
        A + 1.

    fn+ main:
        _ is #[1 1.5 :foo [] [:bar #[]]]
        other module :: multiply 3 by 7
        format "value is ~p" with [C]
        C is (divide 42 by 2).

As you can see there are no commas, no parenthesis and functions receive
parameter "interfixed" between function name tokens, this allows thinks like:

.. code-block:: ruby

    divide 10 by 2
    other module :: multiply 3 by 7
    format "value is ~p" with [C]
    C is (divide 42 by 2).

The code in the previous examples compiles to:

.. code-block:: erlang

    -module(tlfn).
    -export([main/0, export_this_one/1]).

    divide_O_by(A, 0) -> {error, division_by_zero};
    divide_O_by(A, B) -> {ok, A / B}.

    format_O_with(Str, Args) -> io:format(Str, Args).

    export_this_one(0) -> 0;
    export_this_one(A) -> A + 1.

    main() ->
        _ = {1, 1.5, foo, [], [bar, {}]},
        other_module:multiply_O_by(3, 7),
        format_O_with("value is ~p", [C]),
        C = divide_O_by(42, 2).


.. code-block:: erlang

    -module(whenex).
    -export([]).

    main() ->
        if A < 10 -> do_something_with(A), something_else();
           A < 20 -> log_warning("A is ~p", [A]);
           true -> log_warn("wat")
        end.

Build
-----

::

    rebar3 escriptize
    ln -s _build/default/bin/interfix

Run
---

::

    ./interfix erl examples/tlfn.ifx

Status
------

Works
.....

* multi clause functions (no when clauses)
* if expression (when in interfix)
* function calls, local and to other modules
* erlang interop
* ints, floats, atoms, strings
* lists, tuples
* bin, arithmetic, bool operations

Missing
.......

* case .. of (need to think of syntax)
* list comprehension
* record support (need to think of syntax)
* other stuff