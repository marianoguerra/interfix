interfix
========

An experimential programming language for the Erlang VM.

prefix? infix? postfix? none, interfix

underscores? camel case? pascal case? none, interfix

interfix is an experiment to break with some common "expectations" in
programming languages and provide a simpler yet expressive programming language
that is actually useful.

First some working examples:

If
--

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

Misc
----

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

Case Of
-------

.. code-block:: ruby

    fn+ check if A:
        if A is 12: print 12.
        .

    fn+ check if else A:
        if A  is 12: print 12;
        else:        print :other.
        .

    fn+ check if 1 else it A:
        if A  is 12: print 12;
        if it is 32: print 32;
        else:        print :other.
        .

    fn+ check if 2 else it A:
        if A  is 12: print 12;
        if it is 32: print 32;
        if it is 33: print 32;
        else:        print :other.
        .

    fn+ check all A:
        if A  is 12: print 12;
        if it is 32: print 32;
        else:        print :other.
        .

Receive After
-------------

.. code-block:: ruby

    fn+ receive one:
        on message 43: do something here.
        .

    fn+ receive two:
        on message 43: do something here;
        on message :a: something else.
        .

    fn+ receive two and timeout:
        on message 43: do something here;
        on message :a: something else;
        after 50 milliseconds: do timeout thing.
        .

Try Catch Finally
------------------

.. code-block:: ruby

    fn+ try always:
        try:
            something that may break
            something else;
        always:
            try to recover
            and cleanup.
        .

    fn+ try catch:
        try:
            something that may break
            something else;

        catch throw T: handle throw T;
        catch error E: handle error E;
        catch exit Ex: handle exit Ex;
        catch Type E: handle Type E.
        .

    fn+ try catch always:
        try:
            something that may break
            something else;

        catch throw T: handle throw T;
        catch error E: handle error E;
        catch exit Ex: handle exit Ex;
        catch Type E: handle Type E;
            
        always:
            try to recover
            and cleanup.
        .

Begin End
---------

.. code-block:: ruby

    fn+ simple do with A:
        do:
            some stuff with A
            and some other stuff
            A + 2.
        .
            
    fn+ do with A as value:
        with result of do (do:
            some stuff with A
            and some other stuff
            A + 2).

Data Types
----------

.. code-block:: ruby

    fn+ data types examples:
        an int 42
        a float 1.5 
        an atom :foo
        nil is :nil
        booleans are atoms too :true and :false
        an empty list []
        a list with some items [1 2.5 :true :nil [] [:bar]]
        an empty tuple #[]
        a tuple with some items #[1 2.5 :true :nil [] [:bar]]
        a cons (cons 1 [])
        improper list (cons 1 2)
        nested conses (cons 1 (cons 2 []))
        a list string "hi there"
        a binary string 'hi there too'.

As you can see there are no commas, no parenthesis, no reserved keywords and
functions receive parameter "interfixed" between function name tokens, this
allows thinks like:

.. code-block:: ruby

    divide 10 by 2
    other module :: multiply 3 by 7
    format "value is ~p" with [C]
    C is (divide 42 by 2).

The code in the previous examples compiles to:

If Erlang
---------

.. code-block:: erlang

    main() ->
        if A < 10 -> do_something_with(A), something_else();
           A < 20 -> log_warning("A is ~p", [A]);
           true -> log_warn("wat")
        end.

Misc Erlang
------------

.. code-block:: erlang

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


Case Of Erlang
--------------

.. code-block:: erlang

    check_if(A) -> case A of 12 -> print(12) end.

    check_if_else(A) ->
        case A of
          12 -> print(12);
          _ -> print(other)
        end.

    check_if_O_else_it(1, A) ->
        case A of
          12 -> print(12);
          32 -> print(32);
          _ -> print(other)
        end.

    check_if_O_else_it(2, A) ->
        case A of
          12 -> print(12);
          32 -> print(32);
          33 -> print(32);
          _ -> print(other)
        end.

    check_all(A) ->
        case A of
          12 -> print(12);
          32 -> print(32);
          _ -> print(other)
        end.

Receive After Erlang
--------------------

.. code-block:: erlang

    receive_one() -> receive 43 -> do_something_here() end.

    receive_two() ->
        receive
          43 -> do_something_here();
          a -> something_else()
        end.

    receive_two_and_timeout() ->
        receive
          43 -> do_something_here();
          a -> something_else()
          after 50 -> do_timeout_thing()
        end.

Try Catch Finally Erlang
------------------------

.. code-block:: erlang

    try_always() ->
        try something_that_may_break(), something_else() after
          try_to_recover(), and_cleanup()
        end.

    try_catch() ->
        try something_that_may_break(), something_else() catch
          T -> handle_throw(T);
          error:E -> handle_error(E);
          exit:Ex -> handle_exit(Ex);
          Type:E -> handle(Type, E)
        end.

    try_catch_always() ->
        try something_that_may_break(), something_else() catch
          T -> handle_throw(T);
          error:E -> handle_error(E);
          exit:Ex -> handle_exit(Ex);
          Type:E -> handle(Type, E)
        after
          try_to_recover(), and_cleanup()
        end.

Begin End Erlang
----------------

.. code-block:: erlang

    simple_do_with(A) ->
        begin
          some_stuff_with(A), and_some_other_stuff(), A + 2
        end.

    do_with_O_as_value(A) ->
        with_result_of_do(begin
                            some_stuff_with(A), and_some_other_stuff(), A + 2
                          end).

Data Types Erlang
-----------------

.. code-block:: erlang

    data_types_examples() ->
        an_int(42),
        a_float(1.5),
        an_atom(foo),
        nil_is(nil),
        booleans_are_atoms_too_O_and(true, false),
        an_empty_list([]),
        a_list_with_some_items([1, 2.5, true, nil, [], [bar]]),
        an_empty_tuple({}),
        a_tuple_with_some_items({1, 2.5, true, nil, [], [bar]}),
        a_cons([1]),
        improper_list([1 | 2]),
        nested_conses([1, 2]),
        a_list_string("hi there"),
        a_binary_string(<<"hi there too">>).

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
* case .. of
* receive/after
* try/catch/finally
* function calls, local and to other modules
* erlang interop
* ints, floats, atoms, strings, binary strings
* lists, tuples, cons lists
* bin, arithmetic, bool, comparisson operations

Missing
.......

* list comprehension
* record support (need to think of syntax)
* other stuff
