let x := 1 in
let f := proc (x, y, z) 
  (x := x + (y := y + 1) + (z := z + 1))
in
f <x, x, x>;
print (x)
