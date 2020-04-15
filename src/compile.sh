#!/bin/bash

MODE=$1

if [ -z $MODE ]; then
  echo "Unspecified mode, must be either 'compile' or 'run'"
  exit 1
fi

if [ $MODE == "compile" ]; then
# Compile the compiler, then compile the test.cc file into assembly.s.
ocamlopt -o cc_ocaml lexer.ml parser.ml codegen.ml
./cc_ocaml
fi

if [ $MODE == "run" ]; then
# Compile the generated assembly into executable and run.
# Use the brew installed gcc since the default gcc in macos no longer supports 32bit.
/usr/local/Cellar/gcc/9.3.0/bin/gcc-9 -m32 assembly.s -o out
./out
echo "output is $?!"
fi
