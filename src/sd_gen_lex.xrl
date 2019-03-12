Definitions.

Whitespace = [\s\n]
Varname = [a-z]
Digit = [0-9]
Posint = 0|([1-9][0-9]*)

Operator = [\-\+\*/\^\(\)]

Rules.

{Whitespace} : skip_token.
{Posint} : {token, {const, list_to_integer(TokenChars)}}.
{Varname} : {token, {variable, hd(TokenChars)}}.
{Operator} : {token, {list_to_atom(TokenChars), 0}}.

Erlang code.
