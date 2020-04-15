#!/bin/bash

# Compile the compiler, then compile the test.cc file into assembly.s.
ocamlopt -o cc_ocaml lexer.ml parser.ml codegen.ml
./cc_ocaml

# Compile the generated assembly into executable and run.
# /usr/local/Cellar/gcc/9.3.0/bin/gcc-9 -m32 assembly.s -o out
# ./out

