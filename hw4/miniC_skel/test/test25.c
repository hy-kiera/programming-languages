let x := 1 in
let y := 5 in
let f := proc (z) 
  begin
  if (z) { (x := x + 1) } else { (y := y - 2) }
  end
in
let x := 10 in
let y := 50 in
let g := proc (z)
  begin
  if (z) { (x := x + 5) } else { (y := y - 10) }
  end
in
print ((g (true)) + (g (false)) + x + y + (f (true)) + (f (false)) + x + y)
