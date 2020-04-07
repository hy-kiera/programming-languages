(* Exercise 7 - OK *)
let rec fold3 f a l1 l2 l3 =
  match (l1, l2, l3) with
  ([], [], []) -> a
  | (h1::t1, h2::t2, h3::t3) -> fold3 f (f a h1 h2 h3) t1 t2 t3;;