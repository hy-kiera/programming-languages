letrec bin2dec(bin) = 
  proc(depth)
  (
    if iszero bin then 0
    else (
      let remainder = bin-((bin/10)*10) in
      (remainder * depth)+((bin2dec (bin/10)) (depth*2))
    )
  )
in ((bin2dec read) 1)
