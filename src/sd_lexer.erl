-module(sd_lexer).
-export([tokenize/1]).
-export_type([token/0]).

-type token() :: {atom(), unicode:chardata()}.

-spec tokenize(unicode:chardata()) -> {ok, [token()]} | {error, any()}.

tokenize(IoData) -> 
    case unicode:characters_to_binary(IoData) of
        Binary when is_binary(Binary) ->
            do_tokenize(Binary, #{
                          paren_depth => 0
                         }, []);
        _ErrorState ->
            {error, encoding}
    end.

%% Private Functions

-define(is_digit(X), is_integer(X) andalso X =< $9 andalso X >= $0).
-define(is_alpha(X), is_integer(X) andalso ((X =< $z andalso X >= $a) or (X =< $Z andalso X >= $A))).

%{number, Number}
%{op, $-}
%{op, OpChar}
%lparen
%rparen
%{variable, VarName}

do_tokenize(<<>>, _State, Acc) -> {ok, lists:reverse(Acc)};
do_tokenize(<<Char, _Rest/binary>> = Str, State, Acc) when ?is_digit(Char) ->
    {Number, Rest} = read_number(Str, 0),
    do_tokenize(Rest, State, [{const, Number} | Acc]);
do_tokenize(<<Char, Rest/binary>>, State, Acc) when ?is_alpha(Char) ->
    do_tokenize(Rest, State, [{variable, Char - $a} | Acc]);
do_tokenize(<<$+, Rest/binary>>, State, Acc) -> do_tokenize(Rest, State, [plus | Acc]);
do_tokenize(<<$-, Rest/binary>>, State, Acc) -> do_tokenize(Rest, State, [minus | Acc]);
do_tokenize(<<$*, Rest/binary>>, State, Acc) -> do_tokenize(Rest, State, [times | Acc]);
do_tokenize(<<$/, Rest/binary>>, State, Acc) -> do_tokenize(Rest, State, [divide | Acc]);
do_tokenize(<<$(, Rest/binary>>, #{paren_depth := Depth} = State, Acc) ->
    do_tokenize(Rest, State#{paren_depth := Depth+1}, [{lparen, Depth+1} | Acc]);
do_tokenize(<<$), Rest/binary>>, #{paren_depth := Depth} = State, Acc) ->
    do_tokenize(Rest, State#{paren_depth := Depth-1}, [{rparen, Depth} | Acc]);
do_tokenize(<<$^, Rest/binary>>, State, Acc) -> do_tokenize(Rest, State, [exp | Acc]);
do_tokenize(<<$\s, Rest/binary>>, State, Acc) -> do_tokenize(Rest, State, Acc);
do_tokenize(<<$\n, Rest/binary>>, State, Acc) -> do_tokenize(Rest, State, Acc).

read_number(<<>>, Acc) -> {Acc, <<>>};
read_number(<<Char/utf8, Rest/binary>>, Acc) when ?is_digit(Char) -> read_number(Rest, Acc * 10 + (Char - $0));
read_number(<<_Char/utf8, _Rest/binary>> = Str, Acc) -> {Acc, Str}.
