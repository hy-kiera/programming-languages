letrec digitsum(digit) =
  if iszero digit then 0
  else(
    let num  = digit - ((digit/10)*10) in
    num + (digitsum (digit/10))
  )
in (digitsum read)
