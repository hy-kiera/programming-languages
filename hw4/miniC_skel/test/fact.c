/* read n and print n! */
 let x:=0 in
   let y:=1 in 
    read(x);
    while (!(x=1)) {
      (y := y * x);
      (x := x - 1)
    };
  print(y)
