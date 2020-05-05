letrec dfact(n) = 
  if iszero n then 1
  else if iszero (n-1) then 1
  else n*(dfact (n-2))
in (dfact read)
