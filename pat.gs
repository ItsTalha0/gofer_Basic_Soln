-- Print simple line pattern ******* 

p1.x = [ '*' | n<-[0...x],mod.n.2==0 ]


p2.x = map.(\x-> if mod.x.2==0 then ' ' else '+').[0...(x-1)]
p.x = map.(\x-> if mod.x.2==0 then ' ' else '+').[0...(x-1)]
p3.x =concat.[ [' ' | i <- [0...(x-n)]]++p2.(2*n)++['\n'] | n <- [0...(x-1)]]
p4.x =concat.[ [' ' | i <- [0...(x-(n-1))]]++p2.(2*n)++['\n'] | n <- [(x-1),(x-2)...0]]

diamond.n = (p3.n)++(p4.(n-1))

--ls.(' '::x::xs) =

-- Question 1
nZ.l = map.(\(x,y)->x*y).l

-- Question 2
illu. [] . w = [w]
illu.(' '::x::xs).w = if x/=' ' then illu.(xs).(w++[x])     else illu.(x::xs).w
illu.(x::' '::xs).w =if x/=' '  then (w++[x])::illu.(xs).[] else illu.(xs).w

--Question 3

ssl.l = [ head.(tail.x) + (last.x)*(last.x) | x <- l]
ssl2.l = [ y + p*p | (x::y::xs)++[p] <- l]

--Question 4



isSorted.(x::y::[]) = x<y
isSorted.(x::y::xs) = x<y && isSorted.(y::xs)
