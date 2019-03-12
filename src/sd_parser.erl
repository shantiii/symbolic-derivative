-module(sd_parser).
-export([parse/1]).
-export([lalr/1]).

-type ast() :: any().

-spec parse([sd_lexer:token()]) -> {ok, ast()} | {error, any()}.
parse(Tokens) ->
    case expr(lists:reverse(Tokens)) of
        {AST, []} -> {ok, AST};
        {PartialAST, Rest} -> {error, {PartialAST, Rest}}
    end.

%% expr = mul_expr '+' mul_expr
%%        mul_expr '-' mul_expr
%%        mul_expr
%% mul_expr = exp_expr '*' exp_expr
%%          | exp_expr / exp_expr
%%          | exp_expr exp_expr
%%          | exp_expr
%% exp_expr = number '^' number
%%          | number
%% number = const
%%        | variable
%%        | '(' expr ')'

-define(is_add(X), X == '-' orelse X == '+').
-define(is_mul(X), X == '/' orelse X == '/').

% Reversed Recursive-descent parser

expr(Tokens) ->
    case mul_expr(Tokens) of
        {Rhs, [plus|Rest0]} ->
            {Lhs, Rest1} = expr(Rest0),
            {{'+', Lhs, Rhs}, Rest1};
        {Rhs, [minus|Rest0]} ->
            {Lhs, Rest1} = expr(Rest0),
            {{'-', Lhs, Rhs}, Rest1};
        {Expr, Rest} -> {Expr, Rest}
    end.

mul_expr(Tokens) ->
    case exp_expr(Tokens) of
        {Rhs, [times|Rest0]} ->
            {Lhs, Rest1} = mul_expr(Rest0),
            {{'*', Lhs, Rhs}, Rest1};
        {Rhs, [divide|Rest0]} ->
            {Lhs, Rest1} = mul_expr(Rest0),
            {{'/', Lhs, Rhs}, Rest1};
        {Expr, Rest} -> {Expr, Rest}
    end.

exp_expr(Tokens) ->
    case number(Tokens) of
        {Rhs, [exp|Rest0]} ->
            {Lhs, Rest1} = exp_expr(Rest0),
            {{exp, Lhs, Rhs}, Rest1};
        {Expr, Rest} -> {Expr, Rest}
    end.

number([{const, Number},minus|Rest]) -> {{const, -Number}, Rest};
number([minus|Rest0]) ->
    {Expr, Rest1} = pos_number(Rest0),
    {{'*', -1, Expr}, Rest1};
number(Tokens) -> pos_number(Tokens).

pos_number([{const, Number}|Rest]) -> {{const, Number}, Rest};
pos_number([{variable, Name}|Rest]) -> {{variable, Name}, Rest};
pos_number([{lparen, Depth}| Rest0]) ->
    case expr(Rest0) of
        {Expr, [{rparen, Depth}| Rest1]} -> {Expr, Rest1}
    end.

lalr(Tokens) -> lalr([], Tokens).

% Reduction Rules

lalr([{expr, E}], []) -> {ok, E};

lalr([{mul_expr, Rhs},plus,{expr, Lhs}|Stack], Tokens) -> lalr([{expr, {'+', Lhs, Rhs}}|Stack], Tokens);
lalr([{mul_expr, Rhs},minus,{expr, Lhs}|Stack], Tokens) -> lalr([{expr, {'-', Lhs, Rhs}}|Stack], Tokens);
lalr([{mul_expr, E}|Stack], Tokens) -> lalr([{expr, E}|Stack], Tokens);

lalr([{exp_expr, Rhs},times,{expr, Lhs}|Stack], Tokens) -> lalr([{mul_expr, {'*', Lhs, Rhs}}|Stack], Tokens);
lalr([{exp_expr, Rhs},divide,{expr, Lhs}|Stack], Tokens) -> lalr([{mul_expr, {'/', Lhs, Rhs}}|Stack], Tokens);
lalr([{exp_expr, E}|Stack], Tokens) -> lalr([{mul_expr, E}|Stack], Tokens);

lalr([{num, Exp},exp,{expr, Base}|Stack], Tokens) -> lalr([{exp_expr, {'^', Base, Exp}}|Stack], Tokens);
lalr([{num, E}|Stack], Tokens) -> lalr([{exp_expr, E}|Stack], Tokens);

lalr([{pos_num, {const, C}}, minus|Stack], Tokens) -> lalr([{num, {const, -C}}|Stack], Tokens);
lalr([{pos_num, E}, minus|Stack], Tokens) -> lalr([{num, {'*', {const, -1}, E}}|Stack], Tokens);
lalr([{pos_num, E}|Stack], Tokens) -> lalr([{num, E}|Stack], Tokens);

% positive number promotion rules
lalr([{const, _}=E|Stack], Tokens) -> lalr([{pos_num, E}|Stack], Tokens);
lalr([{variable, _}=E|Stack], Tokens) -> lalr([{pos_num, E}|Stack], Tokens);
lalr([{rparen, D}, {expr, E}, {lparen, D}|Stack], Tokens) -> lalr([{pos_num, E}|Stack], Tokens);
% Shift Rule
lalr(Stack, [Token|Ts]) -> lalr([Token|Stack], Ts).
% We're done here.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

% parse_test_() -> gen_tests(fun parse/1).
% lalr_test_() -> gen_tests(fun lalr/1).

gen_tests(TestFun) ->
    Helper = fun({Input, Expected}) ->
                     {ok, Tokens} = sd_lexer:tokenize(Input),
                     ?_assertEqual({ok, Expected}, TestFun(Tokens))
             end,
    Tests =
    [
     {"2", {const, 2}},
     {"x", {variable, $x}},
     {"2+3", {'+', {const, 2}, {const, 3}}},
     {"1-2-3", {'-', {'-', {'-', {const, 1}, {const, 2}}, {const, 3}}, {variable, $x}}}
    ],
    lists:map(Helper, Tests).

-endif.
