## Goal
The goal of this homework is to implement an interpreter for the language with recursive procedures and explicit references.
The syntax and semantics of the language are defined in "hw3.pdf".

## Specification
- Implement the ***eval*** function in the "m.ml".
- Do not modify the types and names of items in the "m.ml" except for the ***eval*** function.
- Use the OCaml's *read_int* function for implementing the *read* expression:
  - http://caml.inria.fr/pub/docs/manual-ocaml/libref/Pervasives.html
- You can use the environment and memory implementation for the interpreter which are provided as built-in.
- During execution, raise the exception *UndefSemantics* whenever the semantics is undefined.
  - e.g.) ADD expressions with a value of Bool type, a conditional expression of type int, etc 
  - You should use the pre-defined exception that we provided in "m.ml".
  - You can raise the exception by ```raise UndefinedSemantics```.

## Compilation and Execution
Compile and execute the interpreter as follows:
```
  make                (* for compilation *)
  ./run test/proc1.m  (* running the interpreter *)
```

## Correctness 
Your program should behave as described in ***test_log***. 

## How to Submit
Submit the single file "m.ml". The file should be compilable. 
