letrec fact(n) = if iszero n then 1 else ((fact (n-1)) * n) in
  (fact read)
