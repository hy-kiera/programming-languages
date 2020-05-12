let f := proc (x, y, z) (x + y + z) in
(f (1, 2, 3) +
        let f := proc (x, y) (x + y) in
        f (1, 2, 3)
)

