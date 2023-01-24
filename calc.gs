-- Simple calculator
----ABs
itemR:[a]->a
itemR.[a]= a

---- Char To int 
ctoi.(c) = ord.c - ord.'0'
itoc.(i) = chr.(i+(ord.'0'))


---- Simple left To Right

calc.(x::42::y::z) = calc.(x*y::z)
calc.(x::43::y::z) = calc.(x+y::z)
calc.(x::45::y::z) = calc.(x-y::z)
calc.(x::47::y::z) = calc.(x/y::z)
calc.[x] = x

--Bodmas Calculator


ltoi:[Char]->[Int]
ltoi.x = map.(\x->if x=='+' || x=='-' || x=='*' || x=='/' then ord.x else ord.x - ord.'0').x

muL.(x::y::z::xs)= if y == 42 then muL.(x*z::xs) else x::y::muL.(z::xs)
muL.[x] = [x]

adD.(x::y::z::xs)= if y == 43 then adD.(x+z::xs) else x::y::adD.(z::xs)
adD.[x] = [x]

suB.(x::y::z::xs)= if y == 45 then suB.(x-z::xs) else x::y::suB.(z::xs)
suB.[x] = [x]

diV.(x::y::z::xs)= if y == 47 then diV.(x/z::xs) else x::y::diV.(z::xs)
diV.[x] = [x]
bodmasCalc:[Char]->Int
bodmasCalc.x =itemR.(adD.(suB.(muL.(diV.(ltoi.x)))))

