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
        [Input] ->
            {ok, Tokens} = sd_lexer:tokenize(Input),
            io:format("Lex: ~p~n", [Tokens]),
            {ok, AST} = sd_parser:parse(Tokens),
            io:format("AST: ~p~n", [AST]);
        _ ->
            io:format("No input provided.")
    end,
    erlang:halt(0).

%%====================================================================
%% Internal functions
%%====================================================================

