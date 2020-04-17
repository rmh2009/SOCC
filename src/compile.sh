#!/bin/bash

MODE=$1

if [ -z $MODE ]; then
  echo "Unspecified mode, must be either 'compile' or 'run'"
  exit 1
fi

if [ $MODE == "compile" ]; then
  # Compile the compiler, then compile the test.cc file into assembly.s.
  ocamlopt -o cc_ocaml lexer.ml parser.ml codegen.ml
  ret=$?
  if [[ $ret -ne 0 ]]; then
    echo "Comiling failed."
    exit 1
  fi
  ./cc_ocaml
fi

if [ $MODE == "run" ]; then
  # Compile the generated assembly into executable and run.
  # Use the brew installed gcc since the default gcc in macos no longer supports 32bit.
  gcc -v -arch i386 assembly.s -o out
  if [ $? -ne 0 ]; then
    exit 1
  fi
  ./cc_ocaml
  ./out
  echo "output is $?!"
fi
