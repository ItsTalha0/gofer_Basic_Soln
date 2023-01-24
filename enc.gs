--simple encryption

--The simple encryptor assumes that the input will be alwas be a lowercase Alphabets,.

--helper functions

ltoi.x = map.(\x->if x /= ' ' then ord.x-ord.'a' else 26).x
itol.x = map.(\x-> (if x== 26 then ' ' else chr.(x+ord.'a'))).x

en:[Int]->[Int]->[Int]->[Int]
en.(26::xs).(y::ys).l = 26::en.(xs).ys.l
en.(x::xs).(y::ys).l = (mod.(x+y).25)::en.(xs).(ys).l
en.x.[].l = en.x.l.l
en.[].x.l = []

-- main encrytp function that takes a string and another string as key and encryps the first string.

enc:[Char]->[Char]->[Char]
enc.x.key = itol.(en.(ltoi.x).(ltoi.(key)).(ltoi.(key)))

de:[Int]->[Int]->[Int]->[Int]
de.(26::xs).(y::ys).l = 26::de.(xs).(ys).l
de.(x::xs).(y::ys).l = (mod.(x-y).25)::de.(xs).(ys).l
de.x.[].l = de.x.l.l
de.[].x.l = []


-- Main decryptor function that takes the ecrypted string and the key and puts out decrypted string.
dec:[Char]->[Char]->[Char]
dec.x.key = itol.(de.(ltoi.x).(ltoi.(key)).(ltoi.(key)))
