let f = proc (x) (let counter = newref(0) 
                  in (setref(counter, deref(counter)+1); deref(counter))) in
let a = (f 0) in
let b = (f 0) in
  (a-b)
