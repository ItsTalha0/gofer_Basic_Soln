msort.[x] = [x]
msort.[x,y] = if length.x < length.y then [y,x] else [x,y]
msort.l   = merges.(msort.(take.(length.l / 2).l)).(msort.(drop.(length.l /2 ).l))
merges.[].[] = []
merges.[].l = l
merges.l.[] = l
merges.l.k = if length.(head.l)  <  length.(head.k) then head.k::merges.l.(tail.k) else head.l::merges.(tail.l).k

sortD2.l = msort.(alsort.l)


alsort.[x] = [x]
alsort.[x,y] = if x > y then [y,x] else [x,y]
alsort.l   = almerge.(alsort.(take.(length.l / 2).l)).(alsort.(drop.(length.l /2 ).l))
almerge.[].[] = []
almerge.[].l = l
almerge.l.[] = l
almerge.l.k = if (head.l) >  (head.k) then head.k::almerge.l.(tail.k) else head.l::almerge.(tail.l).k



bs.[x,y] = if length.x < length.y then [y,x] else [x,y]
bs.(x::y::xs) = if (length.x) <= length.y then y::bs.(x::xs) else x::bs.(y::xs)
bS.l.0 = l
bS.l.n = bS.(bs.l).(n-1)
b.l = bS.l.(length.l)

sortD.l = b.(sort.l)





