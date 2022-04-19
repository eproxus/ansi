-module(colortool).

% API
-export([main/1]).
-export([cli/0]).

%--- API -----------------------------------------------------------------------

main(Args) ->
    % TODO: Do this only if io:printable_range() is 'unicode'
    io:setopts([{encoding, unicode}]),
    cli:run(Args, #{modules => ?MODULE, progname => "colortool"}).

cli() ->
    #{
        handler => fun display/1,
        arguments => [
            #{
                name => backgrounds,
                short => $b,
                long => "-backgrounds",
                type => {custom, fun ranges/1},
                default => [{0, 16}],
                help =>
                    {"[-b BACKGROUNDS]", [
                        "The set of background colors to enumerate ('0..16')"
                    ]}
            },
            #{
                name => foregrounds,
                short => $f,
                long => "-foregrounds",
                type => {custom, fun ranges/1},
                default => [{0, 16}],
                help =>
                    {"[-f FOREGROUNDS]", [
                        "The set of foreground colors to enumerate ('0..16')"
                    ]}
            },
            #{
                name => text,
                short => $t,
                long => "-text",
                type => string,
                help => {"[-t TEXT]", ["The example text to display"]}
            }
        ]
    }.

display(#{backgrounds := BGs, foregrounds := FGs} = Opts) ->
    Text = maps:get(text, Opts, "•••"),
    TextSize = {Text, length(Text)},
    {Columns, Rows} = rows(colorranges(BGs), colorranges(FGs), TextSize),
    io:format(
        grid:format(Rows, #{header => true, columns => Columns, spacer => " "})
    ).

%--- Internal ------------------------------------------------------------------

rows(BGs, FGs, TextSize) ->
    Colors = lists:reverse(
        foldl_ranges(FGs, [], fun(FG, Acc) -> [FG | Acc] end)
    ),
    Rows = lists:reverse(
        foldl_ranges(BGs, [], fun(BG, Acc) ->
            [[integer_to_list(BG)] ++ row(BG, Colors, TextSize) | Acc]
        end)
    ),
    {columns(Colors), Rows}.

columns(Colors) ->
    {Headers, Count} = headers(Colors),
    PseudoFirst = #{index => 1, name => "", align => right},
    PseudoLast = #{index => Count + 1, name => ""},
    [PseudoFirst | Headers] ++ [PseudoLast].

headers([Color | Colors]) ->
    {Middle, Count} = headers(Colors, [], 3),
    {[header(" " ++ integer_to_list(Color), 2) | Middle], Count}.

headers([Color], Acc, Count) ->
    {[header(integer_to_list(Color) ++ " ", Count) | Acc], Count};
headers([Color | Colors], Acc, Count) ->
    headers(Colors, [header(integer_to_list(Color), Count) | Acc], Count + 1).

header(Name, Index) -> #{index => Index, name => Name, align => center}.

row(BG, [FG | FGs], {Text, Size} = TextSize) ->
    Cells = lists:reverse(cells(FGs, TextSize, [])),
    First = grid:cell([ansi:style({bg, BG}), $\s] ++ cell(FG, Text), Size + 1),
    [First | Cells].

cells([FG], {Text, Size}, Acc) ->
    [grid:cell([cell(FG, Text), $\s, ansi:style(bg_default)], Size + 1) | Acc];
cells([FG | Rest], {Text, Size} = TextSize, Acc) ->
    cells(Rest, TextSize, [grid:cell(cell(FG, Text), Size) | Acc]).

cell(FG, Text) ->
    [ansi:style({fg, FG}), Text, ansi:style(default)].

ranges(Ranges) ->
    [range(string:split(R, "..")) || R <- string:split(Ranges, ",", all)].

range([Num]) ->
    Int = list_to_integer(Num),
    {Int, Int};
range([[], []]) ->
    {0, max};
range([[], To]) ->
    {0, list_to_integer(To)};
range([From, []]) ->
    {list_to_integer(From), max};
range([From, To]) ->
    {list_to_integer(From), list_to_integer(To)}.

colorranges(Ranges) -> [colorrange(R) || R <- Ranges].

colorrange({From, max}) -> {From, 255};
colorrange({From, To} = Range) when From >= 0, To =< 255 -> Range;
colorrange({From, To}) -> error({invalid_colorange, {From, To}}).

foldl_ranges([], Acc, _Fun) ->
    Acc;
foldl_ranges([Range | Ranges], Acc, Fun) ->
    foldl_ranges(Ranges, foldl_range(Range, Fun, Acc), Fun).

foldl_range({From, To}, Fun, Acc) when From > To ->
    lists:foldl(Fun, Acc, lists:reverse(lists:seq(To, From)));
foldl_range({From, To}, Fun, Acc) ->
    lists:foldl(Fun, Acc, lists:seq(From, To)).
