-- The mar  works by peeling outer layer and rotating it and then stitcing up back to the inner rotated matrix.
-- There are 3 main functions func-outer peels the layer and rotate functi0n rotates that peeled layer. inner functi0n returns the inner remaining matrix when the outer layer is peeled.
-- Inner left and outer left are helper fuctions that help to stick back the rotated peeled layer.
---------------------Notes on efficiency for future inplace calling functi0n multiple times. So maybe pass the recurrently used values that needs to be calculated as an parameter.

inner:[[a]]->[[a]]
inner.l = [ drop.1.(init.i) | i <- drop.1.(init.l) ]
outer:[[a]]->[a]
outer.l = head.l ++ [ last.i  | i <- drop.1.(init.l) ] ++reverse.(last.l) ++  [ head.i | i <- reverse.( drop.1.(init.l)) ]

rotate:[a]->[a]
rotate.l = [last.l]++init.l

mar:[[a]]->[[a]]
mar.[[x]] = [[x]]
mar.[x] = [rotate.x]                -- function that rotates the matrix once.
mar.l = [ take.(length.(head.l)).(rotate.(outer.l)) ] ++ innerRight.l ++ [reverse.(drop.(length.(head.l) + length.(inner.l)).(take.(2*length.(head.l) + length.(inner.l)).(rotate.(outer.l))))]
innerLeft:[[a]]->[[a]]
innerLeft.l = [ last.(take.i.(mar.(inner.l)))++[ last.(take.(i+(length.(head.l))).(rotate.(outer.l)))] | i<- [1...(length.(inner.l))]]
innerRight:[[a]]->[[a]]
innerRight.l = [ [ last.(take.(length.(outer.l) - i +1).(rotate.(outer.l)))]  ++ last.(take.i.(innerLeft.l)) | i <- [1...(length.(inner.l))]]

marn:[[a]]->Int->[[a]]
marn.l.0 = l
marn.l.n = marn.(mar.l).(n-1)   -- main function that roatates the matrix n times. 








