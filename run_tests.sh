#!/bin/bash

# Tweakable onfigurations.

IS64BIT=1

# Do not change below this line.

MODE=$1
MODE_RUN_OCAML_FILE=0
MODE_ONE_TEST=0
MODE_ALL_TEST=0

if [ $MODE == "ocaml_file" ]; then
  MODE_RUN_OCAML_FILE=1
elif [ $MODE == "one_test" ]; then
  MODE_ONE_TEST=1
elif [ $MODE == "all_test" ]; then
  MODE_ALL_TEST=1
else
  echo "Unrecognized mode: >$MODE<, must be either 'ocaml_file', 'one_test', or 'all_test'"
  exit 1
fi

OCAML_DEPS="../src/lexer.ml ../src/type.ml ../src/typeutil.ml ../src/parser.ml ../src/util.ml ../src/codegen_util.ml ../src/codegen.ml "

# Compile the compiler, then use it to compile the test.cc file into assembly.s.
compile_code() {
  cc_file=$1
  ocaml_main_file=$2
  echo "Testing file $cc_file..."
  echo $PWD
  cp ../test/$cc_file ./test.cc
  if [ $? != 0 ]; then
    echo "Failed to copy file test/$cc_file"
    exit 1
  fi
    ocamlopt -o cc_ocaml $OCAML_DEPS ../src/"$ocaml_main_file"
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
  ./out > std_output.txt
}

failed_tests=""

run_test()  {
  cc_file=$1
  result=$2
  expected_output=$3
  if [ $IS64BIT = 1 ]; then
    compile_code $cc_file "main64.ml"
  else
    compile_code $cc_file "main.ml"
  fi

  if [ $? != 0 ]; then
    echo "Compiling failed!"
    exit 1
  fi
  run_code
  actual_result=$?
  std_output=$(cat std_output.txt)
  if [ $actual_result != $result ]; then
    echo "*********** ( Failed testing $cc_file, expecting $result, actual $actual_result ) "
    failed_tests="$failed_tests ( Failed testing $cc_file, expecting $result, actual $actual_result ) "
  fi
  if [ -z $expected_output ]; then
    echo "skipping expected_output since it's unset."
  else
    if [ $expected_output != "$std_output" ]; then
      echo "*********** ( Failed testing $cc_file, expecting std output $expected_output, actual $std_output ) "
      failed_tests="$failed_tests ( Failed testing $cc_file, expecting std output $expected_output, actual $std_output ) "
    fi
  fi
  echo "************ Test passed for $cc_file"
}

run_all_tests() {
run_test "binary_operators_expect_0.cc"  0
run_test "binary_operators_expect_1.cc"  1 
run_test "binary_operators_expect_9.cc"  9 
run_test "division_test_expect_3.cc"  3 
run_test "multiply_test_expect_15.cc"  15 

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

run_test "for_nested_for_loops_expect_60.cc" 60
run_test "function_call_test1_expect_19.cc" 19
run_test "function_call_test2_expect_21.cc" 21
run_test "function_call_test_fibonacci_expect_5.cc" 5
run_test "print_hello_world.cc" 0 "Hello, World!"

run_test "array_test1_expect_4.cc" 4
run_test "array_2d_test_expect_4.cc" 4
run_test "array_2d_test_expect2_18.cc" 18

run_test "pointer_test1_expect_3.cc" 3
run_test "pointer_test2_expect_10.cc" 10
run_test "pointer_test3_address_of_array_element_expect3.cc" 3
run_test "pointer_test_address_of_address_increasing_expect_4.cc" 4
}

if [ $MODE_ALL_TEST = 1 ]; then
  echo "Running all unit tests."
  cd src

  run_all_tests

  if [ -z $failed_tests ]; then
    echo "All tests passed!"
  else
    echo "Failed tests: $failed_tests"
  fi
  exit 0
fi

if [ $MODE_ONE_TEST = 1 ]; then
  cd src
  echo "Running one test: $2 expecting $3"
  run_test $2 $3
  exit 0
fi

if [ $MODE_RUN_OCAML_FILE = 1 ]; then
  cd src
  TEST_FILE=$2
  ocamlopt -o ../tmp/cc_ocaml $OCAML_DEPS ../src/"$2"
  ../tmp/cc_ocaml
  exit 0
fi
