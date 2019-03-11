-module(symbolic_differentiator).

%% API exports
-export([main/1]).

%%====================================================================
%% API functions
%%====================================================================

%% escript Entry point
main(Args) ->
    io:format("Args: ~p~n", [Args]),
    case Args of
        [Input, [VarName]] ->
            {ok, Tokens} = sd_lexer:tokenize(Input),
            io:format("Lex: ~p~n", [Tokens]),
            {ok, AST} = sd_parser:parse(Tokens),
            io:format("AST: ~p~n", [AST]),
            Var = VarName - $a,
            Derivative = derivative(AST, Var),
            io:format("Derivative: ~p~n", [Derivative]);
        _ ->
            io:format("No input provided.")
    end,
    erlang:halt(0).

%%====================================================================
%% Internal functions
%%====================================================================

derivative({'+', Lhs, Rhs}, V) -> {'+', derivative(Lhs, V), derivative(Rhs, V)};
derivative({'-', Lhs, Rhs}, V) -> {'-', derivative(Lhs, V), derivative(Rhs, V)};
derivative({'*', Lhs, Rhs}, V) -> {'+', {'*', derivative(Lhs, V), Rhs}, {'*', Lhs, derivative(Rhs, V)}};
derivative({'/', Lhs, Rhs}, V) -> {'/', {'-', {'*', derivative(Lhs, V), Rhs}, {'*', Lhs, derivative(Rhs, V)}}, {exp, Rhs, {const, 2}}};
%derivative({exp, {variable, V} = Lhs, Rhs}, V) -> {'*', Rhs, {exp, Lhs, {'-', Rhs, {const, 1}}}};
derivative({exp, Lhs, Rhs}, V) -> {'*', {'*', Rhs, {exp, Lhs, {'-', Rhs, {const, 1}}}}, derivative(Lhs, V)};
derivative({variable, V}, V) -> {const, 1};
derivative({variable, _NotV}, _V) -> {const, 0};
derivative({const, _Number}, _V) -> {const, 0}.

%% TODO: Reducer
%% TODO: Swap specific power rule for general power rule
%% TODO: Pretty printer for reduced itmes
