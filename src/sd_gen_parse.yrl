Nonterminals
 E uminus.

Terminals
  '-' '+' '(' ')' '/' '*' '^' const variable.

Rootsymbol E.

Left 1 '-' '+'.
Left 2 '/' '*'.
Left 3 '^'.

E -> E '+' E : {'+', '$1', '$3'}.
E -> E '-' E : {'-', '$1', '$3'}.
E -> E '/' E : {'/', '$1', '$3'}.
E -> E '*' E : {'*', '$1', '$3'}.
E -> E '^' E : {'^', '$1', '$3'}.

E -> '(' E ')' : '$2'.
E -> const : '$1'.
E -> variable : '$1'.
E -> uminus : '$1'.

uminus-> '-' E : {'*', {const, -1}, '$2'}.

Erlang code.

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").

parse_test_() -> gen_tests(fun parse/1).
% lalr_test_() -> gen_tests(fun lalr/1).

gen_tests(TestFun) ->
    Helper = fun({Input, Expected}) ->
                     {ok, Tokens, _EndLine} = sd_gen_lex:string(Input),
                     ?_assertEqual({ok, Expected}, TestFun(Tokens))
             end,
    Tests =
    [
     {"2", {const, 2}},
     {"x", {variable, $x}},
     {"-x", {'*', {const, -1}, {variable, $x}}},
     {"2+3", {'+', {const, 2}, {const, 3}}},
     {"1-2-3-x", {'-', {'-', {'-', {const, 1}, {const, 2}}, {const, 3}}, {variable, $x}}}
    ],
    lists:map(Helper, Tests).

-endif.
