intml
=====

About
-----

intml is a small ml language I've been implementing as part of learning
about compilers. It's mostly a subset of ocaml but with the main difference so
far, that product types are compact in memory rather than being a chunk of
pointers.

It has a lexer, parser, type-infering type-checker and code generator.
It outputs 64-bit x86 code for windows, macOS and linux; and 32-bit linux arm.

The name came from the plan that it would just support an int type and
calculations, but now strings, lists, sum and product types

