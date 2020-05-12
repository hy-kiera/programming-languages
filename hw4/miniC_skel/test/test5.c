 let x := 2 in
   let y := 1 in
    while (!(x=1)) {
      (y := y * x);
      (x := x - 1);
      while (y <= 10) {
        (y := y + 1)
      }
    };
    print(y)
  

