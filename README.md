# Decaf

This is a repo of yet another Decaf implementation, now in OCaml.

Since this is an implementation following CS143 programming assignments,
and it is on a very early stage of development, the best description is
"CS143 wannable".


## Build

Get OCaml and OPAM: https://realworldocaml.org/install

Get `menhir` as a `ocamlyacc` replacement:

    opam install menhir

Build with GNU Make:

    make


## Testing

    make tests
