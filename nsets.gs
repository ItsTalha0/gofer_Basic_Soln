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

sg.l.0 = []
sg.[].k=[]
sg.x.1 = [[x]]
sg.(x::xs).k = fitl.x.(sg.(xs).(k-1)) ++concat.[ nils.x.(length.p-1).p | p <- sg.(xs).(k)]



allSets : [a] -> [[[a]]]
allSets.l = concat.[ g.l.k | k <- [1...length.l]]


-- Determinant 

vecMin:[Float]->Int->[Float]  								--Type specifier
vecMin.l.n = [ head.(drop.i.l) | i <- [0...((length.l)-1)] , (/=).n.i ] 		--VecMin finds the minor of single vector eg [1,2,3,4].1 = [1,3,4]
minor:[[Float]]->Int->[[Float]]								--Type specifier for minor
minor.l.n = [ vecMin.ls.n | ls <- tail.l ]						--minor finds the minors for all elements of the first row useing vec min
det:[[Float]]->Float									--Type Specifier For det
det.[[a,b],[c,d]]= a*d - c*b								--Det base case for a 2*2 matrix
det.l = sum.[head.(drop.i.(head.l))*(det.(minor.l.i))*((-1.0)^i)| i <- [0...(length.(head.l)-1)]] --det running case -> summation( eij*(-1^i)*minor(of eij))



