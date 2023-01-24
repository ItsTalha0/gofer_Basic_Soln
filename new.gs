
--Print Diagonal

pd:[[Int]]->Int
pd.(ls) = prd.(ls).(0).0

prd:[[Int]]->Int->Int->Int
prd.((x::xs)::y).row.element = if row==element then  x::prd.(xs::y).row.(element+1) else prd.(xs::y).row.(element+1)
prd.([]::y).row.element = prd.(y).(row+1).0
prd.[].row.element= []

--Permutations

--perm:[a]->[[a]]

--p1.[x] = x
--p1.(x::[y]) = [y,x]


--simple diagonal

di.[] = []
di.l = head.(head.l)::di.(map.(tail).(tail.l))
