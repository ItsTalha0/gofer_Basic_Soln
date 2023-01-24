
inner:[[a]]->[[a]]
inner.l = [ tail.(init.i) | i <- tail.(init.l) ]

outer:[[a]]->[a]
outer.l = head.l ++ [ last.i  | i <- drop.1.(init.l) ] ++reverse.(last.l) ++  [ head.i | i <- reverse.( drop.1.(init.l)) ]


rotate:[a]->[a]
rotate.l = [ last.l ] ++ init.l

mar:[[a]]->[[a]]
mar.[[]] = [[]]
mar.[[x]] = [[x]]
mar.([x]::xs) = map.(\x->[x]).(rotate.(concat.([x]::xs)))
mar.[x] = [rotate.x]                -- function that rotates the matrix once.
mar.l = [ take.(length.(head.l)).(rotate.(outer.l)) ] ++ innerRight.l ++ [reverse.(drop.(length.(head.l) + length.(inner.l)).(take.(2*length.(head.l) + length.(inner.l)).(rotate.(outer.l))))]
innerLeft:[[a]]->[[a]]
innerLeft.l = [ last.(take.i.(mar.(inner.l)))++[ last.(take.(i+(length.(head.l))).(rotate.(outer.l)))] | i<- [1...(length.(inner.l))]]
innerRight:[[a]]->[[a]]
innerRight.l = [ [ last.(take.(length.(outer.l) - i +1).(rotate.(outer.l)))]  ++ last.(take.i.(innerLeft.l)) | i <- [1...(length.(inner.l))]]

marn:[[a]]->Int->[[a]]
marn.l.0 = l
marn.l.n = marn.(mar.l).(n-1)   -- main function that roatates the matrix n times. 


