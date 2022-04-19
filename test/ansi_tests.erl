-module(ansi_tests).

-include_lib("eunit/include/eunit.hrl").

-import(ansi, [terminal/1]).

%--- Tests ---------------------------------------------------------------------

mode_test_() ->
    [
        fun() ->
            ?assertEqual(ok, terminal({mode, M})),
            ?assertEqual(M, terminal(mode))
        end
     || M <- [8, 16, 256, true]
    ].

mode_error_test_() ->
    [?_assertError({invalid_mode, M}, terminal({mode, M})) || M <- [false, 19, {foo}]].

style_8_test_() ->
    [
        ?_assertEqual("\e[0m", style(8, reset)),
        ?_assertEqual("\e[0m", style(8, [reset])),
        ?_assertEqual("\e[1m", style(8, bold)),
        ?_assertEqual("\e[21m", style(8, reset_bold)),
        ?_assertEqual("\e[31;1m", style(8, [red, bold])),
        ?_assertEqual("\e[31;1mFoo\e[0m", style(8, [red, bold], "Foo")),
        ?_assertEqual("\e[31;1mFoo\e[0m", style(8, [red, bold], "Foo"))
    ].

style_256_test_() ->
    Style = {fg, 128},
    [
        ?_assertEqual("", style(8, Style)),
        ?_assertEqual("", style(16, Style)),
        ?_assertEqual("\e[38;5;128m", style(256, Style)),
        ?_assertEqual("\e[38;5;128m", style(true, Style)),
        ?_assertError({invalid_style, {fg, 256}}, style(256, {fg, 256}))
    ].

style_true_test_() ->
    Style = {fg, {0, 128, 255}},
    [
        ?_assertEqual("", style(8, Style)),
        ?_assertEqual("", style(16, Style)),
        ?_assertEqual("", style(256, Style)),
        ?_assertEqual("\e[38;2;0;128;255m", style(true, Style))
    ].

%--- Internal ------------------------------------------------------------------

style(Mode, Style) ->
    terminal({mode, Mode}),
    lists:flatten(ansi:style(Style)).

style(Mode, Style, Text) ->
    terminal({mode, Mode}),
    lists:flatten(ansi:style(Style, Text)).
