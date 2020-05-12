let x := 1 in
let f := proc (x, y, z) (x + y + z) in
print (f ((x := x + 1), (x := x + 1), (x := x + 1)))
