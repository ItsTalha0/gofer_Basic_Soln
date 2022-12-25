f.[].[] = []
f.[].l = [l]
f.[x].l = [l++[x]]
f.(x::xs).l = if x <= head.xs then f.(xs).(l++[x]) else (l++[x])::f.(xs).[]


-- list emancipator 

findPush:[(Char,[Int])]->(Char,Int)->[(Char,[Int])]
findPush.((x,a)::ls).(y,z) = if y==x then (x,a++[z])::ls else (x,a)::findPush.(ls).(y,z)
findPush.[].(y,z)= [(y,[z])]


li:[Char]->[(Char,[Int])]->Int->[(Char,[Int])]
li.(x::xs).l.n = li.xs.(findPush.l.(x,n)).(n+1)
li.[].l.n = l


eman.x = li.x.[].0


findL.n.[(x,y)]= x
findL.n.((x,y)::xs) = if head.y == n then x else findL.n.(xs)


popL.n.[]=[]
popL.n.((x,y)::xs) = if head.y == n then if length.y==1 then xs else (x,tail.y)::xs  else (x,y)::popL.n.(xs)


expandT.(x,[y])= [(x,y)]
expandT.(x,(p::xs)) = (x,p)::expandT.(x,(xs))

expandL.[(x,y)] = expandT.(x,y)
expandL.(x::xs) = expandL.(xs)++expandT.(x)

build.((x,y)::xs).n.[] =  if y==n then x::build.(xs).(n+1). 



builder.[].n = []
builder.l.n = (findL.n.l)::builder.(popL.n.l).(n+1)



-- Find the least element in the list and work on that.
-- The assumption made in this solution is that all the indexes upto a certain number exists in the list.
-- The other assumption made is that all the positions upto the last position also exits in the tuple list. 
-- This should not fail as the given input should satisfies the condition that it forms a valid string without any descrepencies and missing characters.

--heaD.[].l.n = if 
