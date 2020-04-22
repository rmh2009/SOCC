#!/bin/bash

# Compile the compiler, then use it to compile the test.cc file into assembly.s.
compile_code() {
  cc_file=$1
  echo "Testing file $cc_file..."
  echo $PWD
  cp ../test/$cc_file ./test.cc
  if [ $? != 0 ]; then
    echo "Failed to copy file test/$cc_file"
    exit 1
  fi
  ocamlopt -o cc_ocaml ../src/lexer.ml ../src/parser.ml ../src/util.ml ../src/codegen.ml
  ret=$?
  if [[ $ret -ne 0 ]]; then
    echo "Compiling failed."
    exit 1
  fi
  ./cc_ocaml
}

run_code() {
  # Compile the generated assembly into executable and run.
  # Use the brew installed gcc since the default gcc in macos no longer supports 32bit.
  gcc -v -arch i386 assembly.s -o out
  if [ $? -ne 0 ]; then
    "Failed to compile assembly into machine code."
    exit 1
  fi
  ./out
}

failed_tests=""

run_test()  {
  cc_file=$1
  result=$2
  compile_code $cc_file
  run_code
  actual_result=$?
  if [ $actual_result != $result ]; then
    echo "*********** ( Failed testing $cc_file, expecting $result, actual $actual_result ) "
    failed_tests="$failed_tests ( Failed testing $cc_file, expecting $result, actual $actual_result ) "
  fi
  echo "************ Test passed for $cc_file"
}

run_all_tests() {
run_test "binary_operators_expect_0.cc"  0
run_test "binary_operators_expect_1.cc"  1 
run_test "binary_operators_expect_9.cc"  9 
run_test "compound_statement2_expect_1.cc"  1 
run_test "compound_statement3_expect_3.cc"  3
run_test "compound_statement4_expect_3.cc" 3
run_test "compound_statement5_expect_2.cc" 2
run_test "compound_statement_expect_4.cc" 4
run_test "conditional_expression_expect_3.cc" 3
run_test "conditional_statement2_expect_2.cc" 2
run_test "conditional_statement_expect_2.cc" 2
run_test "local_variable_expect_24.cc" 24
run_test "unary_operator_expect_0.cc" 0
run_test "for_loop_expect_10.cc" 10
run_test "for_loop2_expect_10.cc" 10
run_test "for_loop3_expect_10.cc" 10
run_test "do_loop_expect_10.cc" 10
run_test "while_loop_expect_10.cc" 10
run_test "break_in_for_loop2_expect_15.cc" 15
run_test "break_in_for_loop_expect_15.cc" 15
run_test "break_inner_compound_expect_5.cc" 5
run_test "continue_in_do_loop2_expect_15.cc" 15
run_test "continue_in_do_loop_expect_15.cc" 15
run_test "continue_in_for_loop2_expect_15.cc" 15
run_test "continue_in_for_loop_expect_15.cc" 15
run_test "continue_inner_loop_expect_15.cc" 15
}

echo "Running all unit tests."
cd src

run_all_tests

if [ -z $failed_tests ]; then
  echo "All tests passed!"
else
  echo "Failed tests: $failed_tests"
fi


