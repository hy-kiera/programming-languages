let x = 200 in
  let f = proc (z) (z-x) in
    let x = 100 in
      let g = proc (z) (z-x) in
        (f 1) - (g 1)
