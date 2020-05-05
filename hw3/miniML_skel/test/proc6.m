let a = 3 in
  let p = proc (x) (x-a) in
    let a = 5 in
      (a - (p 2))
