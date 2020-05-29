proc (c) (let f = proc (x) c in if (f true) then 1 else ((f f) 2))
