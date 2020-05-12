## Goal
The goal of this homework is to implement an interpreter for the language with implict references and automatic garbage collection.
The syntax and semantics of the language are defined in "hw4.pdf".

## Specification
- Exercise 1) Implement the ***eval*** function in the "c.ml".
- Exercise 2) Implement the ***gc*** function in the "c.ml".
- Do not modify the types and names of items in the "c.ml" except for the ***eval*** and ***gc*** function.
- You can use the environment and memory implementation for the interpreter which are provided as built-in.
- During execution, raise the exception *UndefSemantics* whenever the semantics is undefined.
  - e.g.) ADD expressions with a value of Bool type, a conditional expression of type int, etc 
  - You should use the pre-defined exception that we provided in "c.ml".
  - You can raise the exception by ```raise UndefinedSemantics```.

## Compilation and Execution
Compile and execute the interpreter as follows:
```
  make                (* for compilation *)
  ./run test/test13.c  (* running the interpreter *)
  ./run -gc test/test13.c  (* with automatic garbage collection *)
  ./run -print_mem_size test/test13.c  (* print the size of final memory *)
  ./run -gc -print_mem_size test/test13.c  (* print the size of final memory when using garbage collection *)
```

## Correctness 
Your program should behave as described in ***test_log_1*** and ***test_log_2***. 

## How to Submit
Submit the single file "c.ml". The file should be compilable. 
