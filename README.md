# Decaf

This is a repo of yet another Decaf implementation, now in OCaml.

Since this is an implementation following CS143 programming assignments,
and it is on a very early stage of development, the best description is
"CS143 wannable".

The language spec can be found here: <http://web.stanford.edu/class/archive/cs/cs143/cs143.1128/handouts/030%20Decaf%20Specification.pdf>

## How much is done?

- [ ] Lexer
- [ ] Parser
- [ ] Semantic analysis
- [ ] Code generation
- [ ] Runtime?


## Build

Get OCaml and OPAM: https://realworldocaml.org/install

Get `menhir` as a `ocamlyacc` replacement:

    opam install menhir

Build with GNU Make:

    make


## Testing

    make tests
