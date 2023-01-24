--Name: Talha Khan
--Roll:22111058
--course: Msc

-----------------------------encryption & decryption--------------------------------------
--ltoi converts the string into list of ints 
ltoi:[Char]->[Int]
ltoi.x = map.(\x-> ord.x).x

--it converts the list of int back to string
itol:[Int]->[Char]
itol.x = map.(\x-> (chr.(x))).x


--en is the main helper function that takes the string and the key and shifts each character according to key.

en:[Int]->[Int]->[Int]->[Int]
en.(x::xs).(y::ys).l = (mod.(x+y).255)::en.(xs).(ys).l
en.x.[].l = en.x.l.l
en.[].x.l = []


--encryptor
encrypt:[Char]->[Char]->[Char]
encrypt.x.key = itol.(en.(ltoi.x).(ltoi.(key)).(ltoi.(key)))


--de is the main helper function for decryptor that unshifts the encrypted message to get the orignal text.
de:[Int]->[Int]->[Int]->[Int]
de.(x::xs).(y::ys).l = (mod.(x-y).255)::de.(xs).(ys).l
de.x.[].l = de.x.l.l
de.[].x.l = []

--decryptor
decrypt:[Char]->[Char]->[Char]
decrypt.x.key = itol.(de.(ltoi.x).(ltoi.(key)).(ltoi.(key)))

------------------------------------------------------------------------------------------
------------------------------Calculator--------------------------------------------------


-- Simple calculator

----Return single item from list. =
itemR:[a]->a
itemR.[a]= a

---- Char To int
--character to int
ctoi.(c) = ord.c - ord.'0'
--int to character
itoc.(i) = chr.(i+(ord.'0'))


---- Simple left To Right
-- workhorse of the simpleeval
eval.(x::42::y::z) = eval.(x*y::z)
eval.(x::43::y::z) = eval.(x+y::z)
eval.(x::45::y::z) = eval.(x-y::z)
eval.(x::47::y::z) = eval.(x/y::z)
eval.[x] = x
simpleeval.(x) = eval.(ltoim.x)

--Bodmas Calculator

--String to list of integer
ltoim:[Char]->[Int]
ltoim.x = map.(\x->if x=='+' || x=='-' || x=='*' || x=='/' then ord.x else ord.x - ord.'0').x

--Inplace multiplication
muL.(x::y::z::xs)= if y == 42 then muL.(x*z::xs) else x::y::muL.(z::xs)
muL.[x] = [x]

--Inplace Addition
adD.(x::y::z::xs)= if y == 43 then adD.(x+z::xs) else x::y::adD.(z::xs)
adD.[x] = [x]

--Inplace Substraction
suB.(x::y::z::xs)= if y == 45 then suB.(x-z::xs) else x::y::suB.(z::xs)
suB.[x] = [x]

--Inplace Division
diV.(x::y::z::xs)= if y == 47 then diV.(x/z::xs) else x::y::diV.(z::xs)
diV.[x] = [x]

--bodmaseval
bodmaseval:[Char]->Int
bodmaseval.x =itemR.(adD.(suB.(muL.(diV.(ltoim.x)))))
