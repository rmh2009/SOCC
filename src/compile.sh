#!/bin/bash

MODE=$1
COMPILE=0
RUN=0
TEST=0

if [ $MODE == "compile" ]; then
  COMPILE=1
elif [ $MODE == "run" ]; then
  RUN=1
elif [ $MODE == "test" ]; then
  TEST=1
elif [ $MODE == "all" ]; then
  COMPILE=1
  RUN=1
else
  echo "Unrecognized mode: >$MODE<, must be either 'compile', 'run', or 'all'(runs both compile and run)"
  exit 1
fi

if [ $TEST == 1 ]; then
  # Compile the compiler, then use it to compile the test.cc file into assembly.s.
  ocamlopt -o cc_ocaml_test type.ml lexer.ml typeutil.ml  parser.ml util.ml codegen_util.ml codegen.ml test.ml
  ret=$?
  if [[ $ret -ne 0 ]]; then
    echo "Comiling failed."
    exit 1
  fi
  ./cc_ocaml_test
fi

if [ $COMPILE == 1 ]; then
  # Compile the compiler, then use it to compile the test.cc file into assembly.s.
  ocamlopt -o cc_ocaml type.ml lexer.ml typeutil.ml  parser.ml util.ml codegen_util.ml codegen.ml main.ml
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
