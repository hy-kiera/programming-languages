let f = proc (x) x in
  if (f (iszero 0)) then (f 11) else (f 22)
