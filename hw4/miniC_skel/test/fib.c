/* print fib(1) ... fib(30) */
 let x := 0 in
   let y := 1 in
     let z := 0 in
       let i := 0 in
         let n := 0 in
           let j := 1 in
            while (j <= 30) {
              while (i <= j-1) {
                (y := z + y);
                (z := x);
                (x := y);
                (i := i + 1);
              };
              print (y);
              (j := j+1)
            }
          
        
      
    
  

