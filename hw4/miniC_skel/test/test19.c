let x := 0 in
let z :=
  (let x := 1 in (x := 5); ((x := x + 1) * (let x := 2 in (x := x + 1)))) +
  (let y := 2 in 2 * x)
in
print (z)

