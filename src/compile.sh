#!/bin/bash

MODE=$1
COMPILE=0
RUN=0

if [ $MODE == "compile" ]; then
  COMPILE=1
elif [ $MODE == "run" ]; then
  RUN=1
elif [ $MODE == "all" ]; then
  COMPILE=1
  RUN=1
else
  echo "Unrecognized mode: >$MODE<, must be either 'compile', 'run', or 'all'(runs both compile and run)"
  exit 1
fi

if [ $COMPILE == 1 ]; then
  # Compile the compiler, then use it to compile the test.cc file into assembly.s.
  ocamlopt -o cc_ocaml lexer.ml parser.ml util.ml codegen.ml main.ml
  ret=$?
  if [[ $ret -ne 0 ]]; then
    echo "Comiling failed."
    exit 1
  fi
  ./cc_ocaml
fi

if [ $RUN == 1 ]; then
  # Compile the generated assembly into executable and run.
  # Use the brew installed gcc since the default gcc in macos no longer supports 32bit.
  gcc -v -arch i386 assembly.s -o out
  if [ $? -ne 0 ]; then
    exit 1
  fi
  ./out
  echo "output is $?!"
fi
