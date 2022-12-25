--Interleave set version. 
-- The set will be of the form . [[[a],[b]],[[c],[d]]]

--ils:a->[[a]]->Int->[[[a]]]
--ils.x.l.n =((take.n.l)++[(x::head.(drop.n.l))]++drop.(n+1).l)::ils.x.l.(n-1)
--ils.x.[].n = []
--ils.l.x.n =

fitl:a->[[[a]]]->[[[a]]]
fitl.a.[] = []
fitl.a.(x::xs) = ([a]::x)::fitl.a.(xs)

nils : a -> Int -> [[a]] -> [[[a]]]
nils.y.0.l = [[y::head.l]++drop.1.l]
nils.y.i.l = (take.i.l ++ [y::head.(drop.i.l)] ++ drop.(i+1).l)::nils.y.(i-1).l



--g.l.k = if k==length.l then [[[s]]| s<- l] else g.l.k
g : [a] -> Int -> [[[a]]]
g.x.1 = [[x]]
g.(x::xs).k = if k==(length.xs+1) then [[[s]| s<- (x::xs)]] else fitl.x.(g.(xs).(k-1)) ++concat.[ nils.x.(length.p-1).p | p <- g.(xs).(k)]


allSets : [a] -> [[[a]]]
allSets.l = concat.[ g.l.k | k <- [1...length.l]]

