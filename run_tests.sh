#!/bin/bash

# Tweakable onfigurations.

IS64BIT=1

# Do not change below this line.

MODE=$1
MODE_RUN_OCAML_FILE=0
MODE_ONE_TEST=0
MODE_ALL_TEST=0

if [ $MODE == "ocaml_test" ]; then
  MODE_RUN_OCAML_FILE=1
elif [ $MODE == "one_test" ]; then
  MODE_ONE_TEST=1
elif [ $MODE == "all_test" ]; then
  MODE_ALL_TEST=1
else
  echo "Unrecognized mode: >$MODE<, must be either 'ocaml_test', 'one_test', or 'all_test'"
  echo ""
  echo "Usage:"
  echo ""
  echo "  // Run all tests in 64 or 32 bit"
  echo "  run_tests.sh all_test <64|32>"
  echo "  e.g."
  echo "    ./run_tests.sh all_test 64"
  echo ""
  echo "  // Run one test, test_file_name must be in test/ folder."
  echo "  run_tests.sh one_test <test_file_name> <expected_result> <64|32>"
  echo "  e.g."
  echo "    ./run_tests.sh one_test for_loop3_expect_10.c 10 64"
  echo ""
  echo "  // Compile and run the ocaml file, such as test.ml"
  echo "  run_tests.sh ocaml_test <ocaml_file_name>"
  echo "  e.g."
  echo "    ./run_tests.sh ocaml_test test.ml"
  echo ""
  exit 1
fi

OCAML_DEPS="../src/tokens.ml  ../src/type.ml ../src/debug.ml ../src/typeutil.ml ../src/lexer.ml ../src/parser.ml ../src/util.ml ../src/codegen_util.ml ../src/codegen.ml "

# Compile the compiler, then use it to compile the test.c file into assembly.s.
compile_code() {
  cc_file=$1
  ocaml_main_file=$2
  echo "Testing file $cc_file..."
  echo $PWD
  cp ../$cc_file ./test.cc
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
  if [ $IS64BIT = 1 ]; then
    gcc assembly.s -o out
  else 
    gcc -arch i386 assembly.s -o out
  fi

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
    return 1
  fi
  if [ -z $expected_output ]; then
    echo "skipping expected_output since it's unset."
  else
    if [ "$expected_output" != "$std_output" ]; then
      echo "*********** ( Failed testing $cc_file, expecting std output $expected_output, actual $std_output ) "
      failed_tests="$failed_tests ( Failed testing $cc_file, expecting std output $expected_output, actual $std_output ) "
    fi
  fi
  echo "************ Test passed for $cc_file"
}

run_all_tests() {
run_test "test/binary_operators_expect_0.c"  0
run_test "test/binary_operators_expect_1.c"  1 
run_test "test/binary_operators_expect_9.c"  9 
run_test "test/division_test_expect_3.c"  3 
run_test "test/multiply_test_expect_15.c"  15 

run_test "test/test_inc_expect2.c" 2
run_test "test/test_inc2_expect1.c" 1
run_test "test/test_dec_expect1.c" 1
run_test "test/test_dec2_expect2.c" 2

run_test "test/compound_statement2_expect_1.c"  1 
run_test "test/compound_statement3_expect_3.c"  3
run_test "test/compound_statement4_expect_3.c" 3
run_test "test/compound_statement5_expect_2.c" 2
run_test "test/compound_statement_expect_4.c" 4
run_test "test/conditional_expression_expect_3.c" 3
run_test "test/conditional_statement2_expect_2.c" 2
run_test "test/conditional_statement_expect_2.c" 2
run_test "test/local_variable_expect_24.c" 24
run_test "test/unary_operator_expect_0.c" 0
run_test "test/for_loop_expect_10.c" 10
run_test "test/for_loop2_expect_10.c" 10
run_test "test/for_loop3_expect_10.c" 10
run_test "test/do_loop_expect_10.c" 10
run_test "test/while_loop_expect_10.c" 10
run_test "test/break_in_for_loop2_expect_15.c" 15
run_test "test/break_in_for_loop_expect_15.c" 15
run_test "test/break_inner_compound_expect_5.c" 5
run_test "test/continue_in_do_loop2_expect_15.c" 15
run_test "test/continue_in_do_loop_expect_15.c" 15
run_test "test/continue_in_for_loop2_expect_15.c" 15
run_test "test/continue_in_for_loop_expect_15.c" 15
run_test "test/continue_inner_loop_expect_15.c" 15

run_test "test/for_nested_for_loops_expect_60.c" 60
run_test "test/function_call_test1_expect_19.c" 19
run_test "test/function_call_test2_expect_21.c" 21
run_test "test/function_call_pointer_expect_6.c" 6
run_test "test/function_call_test_fibonaci_expect_5.c" 5
run_test "test/print_hello_world.c" 0 "Hello, World!"

run_test "test/array_test1_expect_4.c" 4
run_test "test/array_2d_test_expect_4.c" 4
run_test "test/array_2d_test_expect2_18.c" 18

run_test "test/pointer_test1_expect_3.c" 3
run_test "test/pointer_test2_expect_10.c" 10
run_test "test/pointer_test3_address_of_array_element_expect3.c" 3
run_test "test/pointer_test_address_of_address_increasing_expect_4.c" 4

run_test "test/char_operations_and_print_test.c" 0 "abcdefghij"
run_test "test/char_array_initialization.c" 0 "helloworld!"

# Complex problems.
run_test "coding_problems/8_queen.c" 92
run_test "coding_problems/trading_stocks.c" 13
}

if [ $MODE_ALL_TEST = 1 ]; then
  if [ "$2" = "64" ]; then
    IS64BIT=1
  elif [ "$2" = "32" ]; then
    IS64BIT=0
  fi

  echo "######## Running all unit tests, is_64_bit=$IS64BIT"
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
  if [ $4 = "64" ]; then
    IS64BIT=1
  elif [ $4 = "32" ]; then
    IS64BIT=0
  fi
  echo "######## Running one test: $2 expecting $3, is_64_bit=$IS64BIT"
  cd src
  echo ""
  run_test $2 $3
  exit 0
fi

if [ $MODE_RUN_OCAML_FILE = 1 ]; then
  cd src
  TEST_FILE=$2
  ocamlopt -o ../tmp/cc_ocaml $OCAML_DEPS ../"$2"
  ../tmp/cc_ocaml
  exit 0
fi
