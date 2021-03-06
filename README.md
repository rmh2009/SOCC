# SOCC (Simple OCaml C Compiler)

A simple C compiler written in OCaml.

This is a toy project for studying how to implement a compiler. It consists of three major components: lexer, parser and code generator. The lexer processes source code string into tokens, then the parser parses the tokens into an AST (based on certain grammars and the recursive descent approach). The code generator then walks through the AST and dumps the assembly code.  

Most of the initial code was implemented within two week's free time. Things I learned while implementing this compiler:
 - Exercise in writing relatively complex OCaml code.
 - Functional data structure (List, Map etc.). Now I understand these immutable containers better, and they are pretty cool, especially how they work internally!
 - How AST are generated. The grammar is the key part, and luckily this has been well established for a classic language like C. Still reasoning about why the grammar is set in certain way can be very interesting, at least for someone new to area.
 - The recursive descent algorithm.
 - Assembly code, and how CPU and registers can be leveraged to implement nested expressions, loops etc.

Implemented features:
 - Variable declarations and references.
 - Conditional statements and expressions.
 - For and Do/While loops.
 - Break/Continue statement.
 - Block statements.
 - Function declaration and call.
 - Support for array type. (see note1)
 - A simple type checking system. (see note1)
 - Support for pointer. (see note2)
 - Supports both 32 bit and 64 bit x86 assembly (see note4).
 - Char type and char array initialization. (see note5)
 - Support for struct. (see note6)
 - Malloc and free.

Some features missing:
 - Support for float/double.
 - Macros
 - #include command.
 - And more...

Initially I read Nora's article (see the reference) on how to write a C compiler, but at the time the article stopped at implementing function calls (As of May 2020).
After this I implemented many more features myself, including a simple type checking system, support for arrays, pointers, more data types such as char, string initializations, supporting both 64bit and 32bit assembly, etc. (See notes below for details of implementation)
The stack allocation methods for local and temporary variables are also changed completely to better efficiency and easier migration to 64 bit.

# References:
 - https://norasandler.com/2017/11/29/Write-a-Compiler.html
 - Some note with x86 64 calling conventions: https://www.lri.fr/~filliatr/ens/compil/x86-64.pdf
 - gcc assembly output with -S :).

# Note 1 : The support for array data type.
[May 8 2020]

At this point the basic structure of expression evaluations and flow control has been established, but the type system is still very primitive, i.e. we only have int type. Next I've decided to implement support for array type on my own. For starting point, we should support following operations: array declaration, array reference, and assignment:

```
# Declaration
int a[10];
int m[10][20];

# Assignment
a[0] = 5;
m[0][1] = 6;

# Reference
return a[0];
return m[0][1];

# Array index could be another expression:
return m[a[0]][1];
```

Some assumptions on the supported operations:
 * We only support assigning to an element, array itself is not assignable. i.e. m[0] = 2 is illegal, assuming m is a multidimentional array.
 * Evaluation of an array variable itself gives you the starting address of the array.

Declaration is straight forward, all sizes must be known at compile time, so we can store a list of sizes along with the type of the array element (always int for now). The memory layout for multidimentional arrays is designed so that it it can be treated as an array of nested arrays. i.e., a two dimensional array d2[10][20] can be treated as an array with 10 elements, where each element is an array of 20 elements. The total memory size of the array can also be easily calculated from the definition.

Array element assignment and reference are very similar. For assignment we need to obtain the address of the specific element, while for reference we also need that address, except we will need the value stored in that address. The tricky part is how do we parse a nested array expression, such as d2[0][1]? Also how do we evaluate partial expression such as d2[0] (if a is two dimentional array)?

The solution I initially came up with is to always define array as one dimensional in the AST, which could be nested array. So d2[i][j] can be parsed as (d2[i])[j], which can then be further decomposed into ((a)[i])[j]. So that an array index operation (using the [] operator) can be defined as:

```
IndexOperation(expression1, expression2)
```

where the expression1 should evaluate to the starting address of the array, and expression 2 should be the index. Then d2[i][j] can be expressed as a nested IndexOperation (Var(a) is evaluating a declared variable a)
```
IndexOperation(IndexOperation(Var(d2), Var(i)), Var(j))
```

All seem good and simple, except it didn't really work.. If we try to implement this, then we see that index operation depends on the step size, which depends on the size of the child element. But int our AST above we don't know the index size since expression1 could be anything. Also the evaluation depends on the element type, we want 'd2' to evaluate to the address, 'd2[i]' also evaluate to the address, and only 'd2[i][j]' evaluates to the value (since the type is no longer an array).
We can only do this if we know the type of expression1. So we need to define a data_type type first (in OCaml syntax): 
```
type data_type_t =
| Int
| Array of data_type_t * int
```

Next I decided to modify the expression evaluation function during code generation phase to always return a data_type of the evaluated expression. This is simpler than the alternative of annotating the AST with type information in every expression node (which would require a second pass of the parser phase). This also allows us to do some type checking, such as 'Expecting an array, but got an int', or 'Can not compare array and int etc."

So the pseudo code for evaluating IndexOperation is:
```
function evalute(expression):

  ... # Processing other expressions.
  if expression is IndexOperation(exp1, exp2):
    Array(child_type, size) = evaluate(exp1)  // we got type of exp1.
    ... # push exp1 to stack
    evaluate(exp2) // exp2 is now in register.
    ... # push exp2 to stack.
    step_size = get_data_type_size(child_type)
    ... # Calculate the address of the element using step_size, exp1 and exp2 and push to stack.

    if child_type is Array:
      ... # Return address directly.
    else:
      ... # Return value at the address.
    return child_type
```

For assignment operation a[i][j] = 5, this is parsed as
```
AssignOpertion(ArrayOperation(ArrayOperation(Var(a), Var(i)), Var(j)), Literal(5))
```

This is the pseudo code for the code generation for assignment expression to an array element, which is very similar to the IndexOperation, except we check that the array element is assignable (not another array).

```
function evaluate(expression):
  ... # Processing other expressions.
  if expression is AssignOperation(ArrayOperation(exp1, exp2), exp3):
    Array(child_type, size) = evaluate(exp1)  // we got type of exp1.
    ... # push exp1 to stack
    evaluate(exp2) // exp2 is now in register.
    ... # push exp2 to stack.
    step_size = get_data_type_size(echild_type
    ... # Calculate the address of the element using step_size, exp1 and exp2 and push to stack.
    
    t3 = evaluate(exp3)

    check(t3 = child_type) # check we are assigning to the same type.
    check(child_type != ArrayType) # exp1 must be an 1-d array.
    ... # move the result of exp3 to the saved address.
```

# Note 2 : The support for pointer
[May 9 2020]

Pointer is one of the most interesting features in C. Now let's see how it can be implemented.

For type declarations, I'm not an expert in C, but complex pointer type definitions can be notoriously difficult to parse. Consider this type:

```
int* (*p)[10];
int* *p2[10];
```

The first statement defines a pointer to an array of size 10, containing pointers to int. The second statement defines an array of pointer to pointer to ints... If you find it confusing, the simple rule is to start from the inner most pointer/identifier and try to go right first, until you hit a parentheses, then go left. To make things easier, also since they are rarely used in practical coding, I'll have this limit imposed:

 * We don't support parentheses in type declarations for now.

These are examples of the pointer related declarations we plan to support:
```
// Declaration
int* p;
int* pm[10];
int** pp;
```

The grammar can be expressed as below, where {} means zero or more occurrences, () is for grouping, not the actual parentheses '\('.
```
type_expression = 
(int|char|double|float)(type_expression) {*} (identifier) { '[' number ']' }
```

* A type declaration must have one and only one identifier.
* One and only one of the fundamental types can exist (int, char, float, double), and they must appear on the left most side.

With the above limit/simplification we can write a left-to-right, one pass parser for the type declaration.

Pointer dereference operation is also trivial, for example, '*p' is parsed as DereferenceOp(Var(p)), we just need to check that Var(p) or whatever inner expression returns a pointer type, then we dereference that address.

```
// Pointer dereference
*p = 3;
int b = *p;
int c = **pp;
```

For taking address operation AddressOfOp(exp), we need to check if exp is a declared variable, or an array element, and just return the address there. Taking the address of any other expressions should be illegal (such as address of a temporary expression)

```
// Taking address.
int* p = &a;
int** pp = &p;
p = &m[0];
```

Both AddressOfOp and DereferenceOp should be parsed on the highest priority, i.e. on the 'factor' level, same as the logical negation operation. The code for taking the address of an array element is actually the same for the ArrayIndexOperation, except we always return the address directly.

# Note 3
(TBD) Changing the stack allocation to bulk allocation.
# Note 4

(TBD) We now have support for 64 bit assembly! Refactored a lot of code in the codegen library, details coming.

# Note 5

Added support for char and array char initialization. Example:
```
char test[100] = "hello world!";
```

To implement this a const string needs to be added to the data section, and then call 'memcpy' to copy to the declared variable. This turned out to be very tricky as I encountered two problems:

One is referencing a lable address. We can't just pass the label direclty as it turned out the absolute address is unknown due to PIE (Position Independent Executable).
On 64 bit machine this can be easily solved using the %rip register, we can use it to calculate the actual address of a label, i.e. 'string_label(%rip)'. But on 32 bit machine, current instruction address is not provided (it's not in %eip ..sigh). After searching on web it turns out there is a trick we can use to
get it:
```
f:
    ...
    call temporary_label
temporary_label:
    pop %eax                # now %eax holds the address of temporary_label.
    leal string_label-temporary_label(%eax), %eax
                            # now %eax holds the address of string_label.

.section    __TEXT,__const
string_label:
    .asciz "hello world"
```

The other issue is alignment before calling a function. %esp/%rsp should be 16 byte aligned right before the call function. _memcpy is especially picky about this. In my previous implementation I make sure that space allocated for a function is 16 byte aligned, but I forgot the fact that a function has two additional pushes on the stack, e.g.

```
f:
  # call f already pushed a return address to stack, so this is +1
  pushl    %ebp
  movl     %esp, %ebp # another push, so +2
```

memcpy accepts 3 parameters, so we need to push another 4 byte to align the stack.

# Note 6

(TBD) Implementing support for struct.

# Compiling the assembly

You can use gcc to compile the assembly output into machine code. This has only been tested on MacOS.

For compiling to 64 bit assembly, use the main64.ml file.

For compiling to 32 bit assembly, use the main.ml file. For MacOS the lasted XCode on MacOS has dropped support for generating 32bit programs, for a temporary solution you can use a virtual box with a 32bit os, or downgrade XCode to a previous version that still had support for 32bit, I used XCode 9.4. The corresponding CommandlineTools need to be installed as well.

# Test

Run the run_tests.sh script to compile a set of test files and compare with expected results.
```
# Run all tests in 64 bit mode.
./run_tests.h all_test 64

# Run all tests in 32 bit mode.
./run_tests.h all_test 32
```
