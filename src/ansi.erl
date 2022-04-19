-module(ansi).

% API
-export([style/1]).
-export([style/2]).
-export([clear/1]).
-export([cursor/1]).
-export([screen/1]).
-export([terminal/1]).
-export([detect_mode/0]).

-on_load(on_load/0).

-define(format(N, Format, Code),
format(Mode, Format) when Mode >= N -> Code;
format(_Mode, Format) -> []
).

%--- API -----------------------------------------------------------------------

style(Format) ->
    case persistent_term:get(ansi_term, undefined) of
        no_color ->
            [];
        Mode ->
            case sequence(Mode, Format) of
                [] -> [];
                Sequence -> [control_sequence(Sequence), $m]
            end
    end.

style(Format, IOData) -> [style(Format), IOData, style(reset)].

clear(Format) -> control_sequence(clear_(Format)).

clear_(display) -> $J;
clear_(to_end_of_screen) -> "0J";
clear_(to_beginning_of_screen) -> "1J";
clear_(screen) -> "2J";
clear_(saved_lines) -> "3J";
clear_(in_line) -> $K;
clear_(end_of_line) -> "0K";
clear_(beginning_of_line) -> "1K";
clear_(line) -> "2K";
clear_(Format) -> error({invalid_erase, Format}).

cursor(Format) -> control_sequence(cursor_(Format)).

cursor_(home) ->
    $H;
cursor_({Direction, 0}) when
    Direction == up; Direction == down; Direction == left; Direction == right
->
    [];
cursor_({up, Lines}) when Lines > 0 ->
    [integer_to_list(Lines), $A];
cursor_({up, Lines}) when Lines < 0 ->
    cursor_({down, abs(Lines)});
cursor_({down, Lines}) when Lines > 0 ->
    [integer_to_list(Lines), $B];
cursor_({down, Lines}) when Lines < 0 ->
    cursor_({up, abs(Lines)});
cursor_({left, Lines}) when Lines > 0 ->
    [integer_to_list(Lines), $C];
cursor_({left, Lines}) when Lines < 0 ->
    cursor_({right, abs(Lines)});
cursor_({right, Lines}) when Lines > 0 ->
    [integer_to_list(Lines), $D];
cursor_({right, Lines}) when Lines < 0 ->
    cursor_({left, abs(Lines)});
cursor_({Direction, beginning, 0}) when Direction == up; Direction == down ->
    [];
cursor_({down, beginning, Lines}) when is_integer(Lines), Lines > 0 ->
    [integer_to_list(Lines), $E];
cursor_({down, beginning, Lines}) when Lines < 0 ->
    cursor_({up, beginning, abs(Lines)});
cursor_({up, beginning, Lines}) when is_integer(Lines), Lines > 0 ->
    [integer_to_list(Lines), $F];
cursor_({up, beginning, Lines}) when Lines < 0 ->
    cursor_({down, beginning, abs(Lines)});
cursor_({column, Column}) when is_integer(Column), Column > 0 ->
    [integer_to_list(Column), $G];
cursor_({line, Line}) when is_integer(Line), Line > 0 ->
    [integer_to_list(Line), $d];
cursor_({Line, Column}) when
    is_integer(Line), is_integer(Column), Line > 0, Column > 0
->
    [integer_to_list(Line), $;, integer_to_list(Column), $H];
cursor_(up_scroll) ->
    $M;
cursor_(save) ->
    $s;
cursor_(restore) ->
    $u;
cursor_(invisible) ->
    "?25l";
cursor_(visible) ->
    "?25h";
cursor_(Format) ->
    error({invalid_cursor, Format}).

screen(Format) -> control_sequence(screen_(Format)).

screen_(save) ->
    "?47h";
screen_(restore) ->
    "?47l";
screen_({alternate_buffer, enable}) ->
    "?1049h";
screen_({alternate_buffer, disable}) ->
    "?1049l";
screen_({scrolling_region, Top, Bottom}) when Top >= 1 ->
    [integer_to_list(Top), $;, integer_to_list(Bottom), $r];
screen_({scrolling_region, reset}) ->
    $r;
screen_(Format) ->
    error({invalid_screen, Format}).

terminal(height) ->
    {ok, Rows} = io:rows(),
    Rows;
terminal(width) ->
    {ok, Columns} = io:columns(),
    Columns;
terminal(mode) ->
    persistent_term:get(ansi_term, undefined);
terminal({mode, Mode}) when
    Mode =:= 8; Mode =:= 16; Mode =:= 256; Mode == true; Mode == no_color
->
    persistent_term:put(ansi_term, Mode);
terminal({mode, Mode}) ->
    error({invalid_mode, Mode}).

detect_mode() ->
    case terminal(mode) of
        undefined ->
            Mode =
                case os:getenv("NO_COLOR") of
                    false ->
                        case os:getenv("COLORTERM") of
                            "truecolor" ->
                                true;
                            _Else ->
                                case tput_colors() of
                                    N when N =:= 8; N =:= 16; N =:= 256 -> N;
                                    N when N > 256 -> true;
                                    false -> term_color(os:getenv("TERM"))
                                end
                        end;
                    _ ->
                        no_color
                end,
            terminal({mode, Mode});
        _Mode ->
            ok
    end.

%--- Callbacks -----------------------------------------------------------------

on_load() -> detect_mode().

%--- Internal ------------------------------------------------------------------

control_sequence(Sequence) -> [$\e, $[, Sequence].

sequence(Mode, [Format]) ->
    sequence(Mode, Format);
sequence(Mode, [Format | Rest]) ->
    case format(Mode, Format) of
        [] -> sequence(Mode, Rest);
        Code -> [Code, $;, sequence(Mode, Rest)]
    end;
sequence(Mode, Format) ->
    format(Mode, Format).

% Modes
?format(8, bold, "1");
?format(8, dim, "2");
?format(8, faint, "2");
?format(8, italic, "3");
?format(8, underline, "4");
?format(8, blinking, "5");
?format(8, inverse, "7");
?format(8, reverse, "7");
?format(8, hidden, "8");
?format(8, invisible, "8");
?format(8, strikethrough, "9");
?format(8, reset_bold, "21");
?format(8, reset_dim, "22");
?format(8, reset_faint, "22");
?format(8, reset_italic, "23");
?format(8, reset_underline, "24");
?format(8, reset_blinking, "25");
?format(8, reset_inverse, "27");
?format(8, reset_reverse, "27");
?format(8, reset_hidden, "28");
?format(8, reset_invisible, "28");
?format(8, reset_strikethrough, "29");
?format(8, reset, $0);
% Foreground
?format(8, black, "30");
?format(8, red, "31");
?format(8, green, "32");
?format(8, yellow, "33");
?format(8, blue, "34");
?format(8, magenta, "35");
?format(8, cyan, "36");
?format(8, white, "37");
?format(8, default, "39");
% Background
?format(8, bg_black, "40");
?format(8, bg_red, "41");
?format(8, bg_green, "42");
?format(8, bg_yellow, "43");
?format(8, bg_blue, "44");
?format(8, bg_magenta, "45");
?format(8, bg_cyan, "46");
?format(8, bg_white, "47");
?format(8, bg_default, "49");
?format(16, bright_black, "90");
?format(16, bg_bright_black, "100");
?format(16, bright_red, "91");
?format(16, bg_bright_red, "101");
?format(16, bright_green, "92");
?format(16, bg_bright_green, "102");
?format(16, bright_yellow, "93");
?format(16, bg_bright_yellow, "103");
?format(16, bright_blue, "94");
?format(16, bg_bright_blue, "104");
?format(16, bright_magenta, "95");
?format(16, bg_bright_magenta, "105");
?format(16, bright_cyan, "96");
?format(16, bg_bright_cyan, "106");
?format(16, bright_white, "97");
?format(16, bg_bright_white, "107");
format(Mode, {fg, N}) when Mode >= 256, N =< 255 ->
    ["38;5;", integer_to_list(N)];
format(_Mode, {fg, N}) when N =< 255 ->
    [];
format(Mode, {bg, N}) when Mode >= 256, N =< 255 ->
    ["48;5;", integer_to_list(N)];
format(_Mode, {bg, N}) when N =< 255 ->
    [];
format(Mode, {fg, {R, G, B}}) when Mode >= true, R =< 255, G =< 255, B =< 255 ->
    [
        "38;2;",
        integer_to_list(R),
        $;,
        integer_to_list(G),
        $;,
        integer_to_list(B)
    ];
format(_Mode, {fg, {R, G, B}}) when R =< 255, G =< 255, B =< 255 ->
    [];
format(Mode, {bg, {R, G, B}}) when Mode >= true, R =< 255, G =< 255, B =< 255 ->
    [
        "48;2;",
        integer_to_list(R),
        $;,
        integer_to_list(G),
        $;,
        integer_to_list(B)
    ];
format(_Mode, {bg, {R, G, B}}) when R =< 255, G =< 255, B =< 255 ->
    [];
format(_Mode, bel) ->
    7;
format(_Mode, Format) ->
    error({invalid_style, Format}).

tput_colors() ->
    case os:find_executable("tput") of
        false ->
            false;
        _Tput ->
            case string:to_integer(string:trim(os:cmd("tput colors"))) of
                {Int, _Rest} -> Int;
                _Error -> false
            end
    end.

term_color("screen-256color") ->
    color_256;
term_color("xterm-kitty") ->
    color_256;
term_color(_Term) ->
    io:format("TERM: ~p~n", [_Term]),
    no_color.
