
insertAtn.[].n.element = [element]
insertAtn.(x::xs).n.element = if n==0 then element::x::xs else x::insertAtn.(xs).(n-1).element

insertAtall.ch.ls.0 = [insertAtn.ls.0.ch]
insertAtall.ch.ls.n = insertAtn.ls.n.ch::insertAtall.ch.ls.(n-1)

perm.l.0= []
perm.(x::xs).n = insertAtall.x.(xs).(length.(xs))++perm.(xs++[x]).(n-1)

permute.x = perm.x.(length.x)
