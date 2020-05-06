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
 - Support for array type. (see notes)
 - A simple type checking system. (see notes)

Limitations:
 - Only has int data type.
 - For now it only compiles to 32 bit x86 assembly code.

Features missing:
 - Support for pointers.
 - Support for struct.
 - Data types other than int, such as double/char etc.

So far I've been mostly following this article here: https://norasandler.com/2017/11/29/Write-a-Compiler.html. This article stopped at implementing function calls (As of May 2020). After this I implemented a simple type checking system, support for arrays etc. on my own. I plan to keep adding more features gradually and will summarize my solution below.

# Note 1 : The support for array data type.
TODO

# Compiling the assembly

You can use gcc to compile the assembly output into machine code. This has only been tested on MacOS. Unfortunately the lasted XCode on MacOS has dropped support for generating 32bit programs, for a temporary solution you can use a virtual box with a 32bit os, or downgrade XCode to a previous version that still had support for 32bit, I used XCode 9.4.

# Test

Run the run_tests.sh script to compile a set of test files and compare with expected results.
