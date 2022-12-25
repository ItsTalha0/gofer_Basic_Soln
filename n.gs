


--Collate takes the key and add it to alternate positions in string.
--Like if the input string is abc and key is xy collate will return axbyc
collate:[Char]->[Char]->[Char]->[Char]
collate.[].l.y = []     -- 1..when the string is empty base case.
collate.l.[].y = collate.l.y.y  --2..when key lenth is smaller than input string. key will berepeated it covers the whole string. 
collate.(x::xs).(y::ys).l = x::y::collate.(xs).(ys).l -- 3..collate function just add each element of key in alternate positions of the string. 


-- encrypt function  will take a key and string and pass the reverse of the string to collate that in turn will return the collated of encrypted string return.
encrypt.string.key = collate.(reverse.string).key.key -- Main encrypt function.



uncollate.[].l.y = [] -- same as 1 
uncollate.x.[].l = uncollate.x.l.l -- same as 2  
uncollate.(x::y::xs).(z::zs).l = if y == z then x::uncollate.(xs).zs.l else x::y::uncollate.(xs).(zs).l -- this does exact opposite of 3 . The beauty of function is that if key is not right you wont get the actual string.

decrypt.x.key  = reverse.(uncollate.x.key.key) --Main decrypt function.

