symbolic_differentiator
=====

An erlang escript application to symbolically differentiate a polynomial with
respect to one variable. Standard mathematical notation applies, except that
all operations are right associative instead of left. Variables can only be one
letter. There is no implicit multiplication when terms are adjacent to each other..

Build
-----

    $ rebar3 escriptize

Run
---

    $ _build/default/bin/symbolic_differentiator <expression> <variable>

To-Do
-----

* [ ] Consistency: use `^` instead of `exp` as the atom for exponentiation.
* [ ] Real Number Support: parse real number inputs as floats.
* [ ] Logarithms: Add the ability to lex, parse, and derive logarithms.
* [ ] Power rule: Switch to using the generalized power rule `u(x)^v(x)`
* [ ] Reduction: The output is pretty difficult to follow most of the time; Add
  mathematical reduction for combinations
* [ ] Associativity fix: The output right now is set to be right associative,
  probably because of the recursive descent parsing. Changing to a LALR parser
  (or even by reversing the RD token stream) Alloys 
