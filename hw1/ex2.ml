(* Exercise 2 - OK *)
let rec range lower upper =
  if lower = upper then [lower]
  else if lower > upper then []
  else [lower] @ range (lower+1) upper;;