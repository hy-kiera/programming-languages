let x = newref (newref (0)) in
  setref (deref(x), 11); deref(deref(x))
