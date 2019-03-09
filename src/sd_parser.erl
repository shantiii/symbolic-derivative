-module(sd_parser).
-export([parse/1]).

-type ast() :: any().

-spec parse([sd_lexer:token()]) -> {ok, ast()} | {error, any()}.
parse(Tokens) ->
    case expr(Tokens) of
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

expr(Tokens) ->
    case mul_expr(Tokens) of
        {Lhs, [plus|Rest0]} ->
            {Rhs, Rest1} = expr(Rest0),
            {{'+', Lhs, Rhs}, Rest1};
        {Lhs, [minus|Rest0]} -> % TODO: replace this with catch test for mul_exp
            case expr(Rest0) of
                {{BinOp, RLhs, RRhs}, Rest1} when ?is_add(BinOp) -> {{BinOp, {'-', Lhs, RLhs}, RRhs}, Rest1};
                {Rhs, Rest1} -> {{'-', Lhs, Rhs}, Rest1}
            end;
        {Expr, Rest} -> {Expr, Rest}
    end.

mul_expr(Tokens) ->
    case exp_expr(Tokens) of
        {Lhs, [times|Rest0]} ->
            {Rhs, Rest1} = mul_expr(Rest0),
            {{'*', Lhs, Rhs}, Rest1};
        {Lhs, [divide|Rest0]} ->
            {Rhs, Rest1} = mul_expr(Rest0),
            {{'/', Lhs, Rhs}, Rest1};
        {Expr, Rest} -> {Expr, Rest}
    end.

exp_expr(Tokens) ->
    case number(Tokens) of
        {Lhs, [exp|Rest0]} ->
            {Rhs, Rest1} = exp_expr(Rest0),
            {{exp, Lhs, Rhs}, Rest1};
        {Expr, Rest} -> {Expr, Rest}
    end.

number([minus,{const, Number}|Rest]) -> {{const, -Number}, Rest};
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

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").
-endif.
