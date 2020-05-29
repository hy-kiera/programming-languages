let makemult = 
  proc (maker)
    proc (x)
      if iszero (x) then 0 
      else (((maker maker) (x-1)) + 4) in
  let times4 = proc (x) ((makemult makemult) x) in
    (times4 read)
