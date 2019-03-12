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
            {ok, Tokens, 1} = sd_gen_lex:string(Input),
            io:format("Lex: ~p~n", [Tokens]),
            {ok, AST} = sd_gen_parse:parse(Tokens),
            io:format("AST: ~p~n", [AST]),
            Derivative = derivative(AST, VarName),
            io:format("Deriv AST: ~p~n", [Derivative]),
            PrettyDerivative = print(Derivative),
            io:format("Derivative: ~s~n", [PrettyDerivative]);
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
derivative({'^', Lhs, Rhs}, V) -> {'*', {'*', Rhs, {'^', Lhs, {'-', Rhs, {const, 1}}}}, derivative(Lhs, V)};
derivative({variable, V}, V) -> {const, 1};
derivative({variable, _NotV}, _V) -> {const, 0};
derivative({const, _Number}, _V) -> {const, 0}.

precedence(const) -> -1;
precedence(variable) -> -1;
precedence('+') -> 0;
precedence('-') -> 0;
precedence('*') -> 1;
precedence('/') -> 1;
precedence('^') -> 2.

parenthize(Parent, {BinOp, _L, _R}=Term) -> [$(, print(Term), $)];
parenthize(Parent, Term) -> print(Term).

print({BinOp, Lhs, Rhs}) -> [parenthize(BinOp, Lhs), atom_to_list(BinOp), parenthize(BinOp, Rhs)];
print({variable, Var}) -> <<Var>>;
print({const, Number}) -> integer_to_binary(Number).

%% TODO: Reducer
%% TODO: Swap specific power rule for general power rule
%% TODO: Pretty printer for reduced itmes
