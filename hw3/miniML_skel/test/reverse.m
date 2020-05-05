letrec reverse(n) = 
  proc (result) 
  (
    if iszero n then result 
    else ( 
      let remainder = (n - ((n/10)*10)) in
      ((reverse (n/10))((result * 10)+remainder))
    )
  )
  in ((reverse read) 0)
