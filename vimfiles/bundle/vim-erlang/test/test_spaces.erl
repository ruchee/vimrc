%%% vim: expandtab tabstop=4 shiftwidth=4

-module(test).

-export([foo/0,
         baaaaar/2,
         baaaaaaaaa/4
        ]).

-export([
    foo/1,
    baaaaar/2,
    baaaaaaaaa/4]).

-define(FOO,
        666 + 777).

-vsn(
    "My beta version"
    ).

-spec foo(X) -> {ok, Key} | {false, X} when
    X :: iter(),
    Key :: integer().

bar() ->
    begin
        more(),
        any()
    end.

bar() ->
    Foo = begin
        more(),
        begin
            no()
        end,
        any()
    end,
    case neee() of
        1 ->
            begin
                more()
            end;
        _ ->
            fdf
    end.

foo() ->
    L = [1, 2
         3
        ],
    L2 = lists:map(fun(N) ->
                    N + 1
            end, L),
    L3 = lists:map(
            fun(N) ->
                    N + 2
            end,
            L),
    L4 = lists:map(
            fun
                (1) ->
                    N + 1;
                (2) ->
                    N + 2;
                (_) ->
                    N
            end, L).

foo() ->
    case
        {foooooooooooooooooo,
         baaaaaaaaaaaaaaaaar}
    of
        ok ->
            ok
    end,
    X = [{a, b}, c,
         {d, e, f,
          g}],
    Y = <<X, X, X,
          X, X, X>>,
    [{X, Y, Z} || X <- L1,
                  Y <- L2,
                  Z <- L3],
    [{X, Y, Z} ||
        X <- L1,
        Y <- L2,
        Z <- L3],
    [{X, Y, Z}
     || X <- L1,
        Y <- L2,
        Z <- L3],
    Foo = [{X, Y, Z} || X <- L1,
                        Y <- L2,
                        Z <- L3],
    Foo = [{X, Y, Z} ||
            X <- L1,
            Y <- L2,
            Z <- L3],
    Foo = [{X, Y, Z}
           || X <- L1,
              Y <- L2,
              Z <- L3],
    Z = [
            begin
                some(X, Y)
            end || X <- L1,
                   Y <- L2].

foo() ->
    bar(fun foo/0,
        1,
        2,
        3
       ),
    X = 1 + 2 +
        3 + 4,
    Y = case foo() of
        foo ->
            bar()
    end,
    ok.

foo() ->
    fuuuuuuuuuuuuur(
        1,
        2
        ),
    ok.

fooooooooo(X,
           Y) when
        X =:= Y ->
    ok.

foo() ->
    case foo() of
        bar ->
            catch fii();
        fuu() ->
            ber()
    end.

foo() ->
    case foo() of
        bar ->
            X = catch fii();
        fuu() ->
            ber()
    end.

foo() ->
    X = try
        foo()
    catch
        foo when
                foo ->
            bar()
    after
        bar()
    end.

foo() ->
    try
        foo()
    catch
        foo when
                foo ->
            bar()
    after
        bar()
    end.

foo() ->
    try
        foo(),
        bar()
    of
        foo when
                bar == 2 ->
            foo(),
            bar();
        bar ->
            foo
    end.

foo() ->
    try
        foo(),
        bar()
    of
        foo when
                bar == 2 ->
            foo(),
            bar();
        bar ->
            foo
    after
        foo(),
        bar()
    end.

foo() ->
    try
        foo(),
        bar()
    of
        foo when
                bar == 2 ->
            foo(),
            bar();
        bar ->
            foo
    after
        foo(),
        bar()
    end.

foo() ->
    try
        foo(),
        bar()
    of
        foo when
                bar == 2 ->
            foo(),
            bar();
        bar ->
            foo
    catch
        foo when
                foo ->
            foo
    after
        foo(),
        bar()
    end.

foo() ->
    receive
    after
        1000 ->
            bar()
    end.

foo() ->
    receive
        foo when
                foo ->
            foo()
    end.

foo() ->
    receive
        foo when
                foo ->
            foo()
    after
        1000 ->
            bar()
    end.

foo() ->
    if
        foo ->
            bar();
        bar ->
            foo()
    end.

foo() ->
    case foo() of
        foo when
                bar ->
            foo();
        bar ->
    end.

foo() ->
    case
        foo()
    of
        foo when
                bar ->
            foo();
        bar ->
    end.
