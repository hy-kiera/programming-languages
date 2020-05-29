# HW5

## Goal
The goal is to implement a static type system for the language with recursive procedures.
- the simple type system
- the polymorphic type system with inlining (page 4 of Lecture 16)
Read "hw5.pdf" carefully.

## Specification
- Problem 1) Implement the ***gen_equations*** and ***solve*** functions in the "m.ml".
- Problem 2) Implement the ***expand*** function in the "p.ml".
- Do not modify anything in files "m.ml" and "p.ml" except for the those three functions. (You can add additional helper functions as necessary)
- During type checking, raise the exception *TypeError* whenever type checking should fail.
  - You should use the pre-defined exception that we provided in "m.ml".
  - You can raise the exception by ```raise TypeError```.

## Compilation and Execution
Compile and execute the type checker as follows:
```
  make                   (* for compilation *)
  ./run test/example1.m  (* running the simple type checker *)
```

If everything is successful, you will obtain the following output:

```
= Program =
proc (f) proc (x) ((f 3) - (f x))

= Equations =
t1 = (t2 -> t3)
t3 = (t4 -> t5)
t5 = int
(t7 -> int) = t2
t7 = int
(t6 -> int) = t2
t6 = t4

= Substitution =
t4 |-> int
t6 |-> int
t7 |-> int
t2 |-> (int -> int)
t5 |-> int
t3 |-> (int -> int)
t1 |-> ((int -> int) -> (int -> int))

Type of the given program: ((int -> int) -> (int -> int))
```

You can execute the polymorphic type checker as follows:
```
  ./run -poly test/poly.m  (* running the polymorphic type checker *)
```
If everything is successful, you will obtain the following output:
```
= Program = 
let f = proc (x) x in
  if (f (iszero (0)))
  then (f 11)
  else (f 22)

= Equations = 
(t8 -> bool) = (t9 -> t10)
t10 = t9
t8 = bool
int = int
(t5 -> t1) = (t6 -> t7)
t7 = t6
t5 = int
(t2 -> t1) = (t3 -> t4)
t4 = t3
t2 = int

= Substitution = 
t3 |-> int
t4 |-> int
t2 |-> int
t6 |-> int
t7 |-> int
t1 |-> int
t5 |-> int
t9 |-> bool
t10 |-> bool
t8 |-> bool

Type of the given program: int
```
The same program does not pass the simple type checker, though. 
```
  ./run test/poly.m  (* running the simple type checker *)
```
```
= Program = 
let f = proc (x) x in
  if (f (iszero (0)))
  then (f 11)
  else (f 22)

= Equations = 
t2 = (t6 -> t7)
t7 = t6
(t5 -> bool) = t2
t5 = bool
int = int
(t4 -> t1) = t2
t4 = int
(t3 -> t1) = t2
t3 = int

The program does not have type. Rejected.
```

