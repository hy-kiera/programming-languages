let x := {i1 := 1, i2 := 2, i3 := {ii1 := true, ii2 := skip} } in
if (x.i3.ii2 = skip) { print (100) } else { print (200) }

