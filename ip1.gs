-- Question 1 seq Find
f.[].l = True
f.l.[] = False
f.(x::xs).(y::ys) = if x==y then f.(xs).(ys) else f.(x::xs).(ys)




-- Question 2 sublist 
f2.[].[] = True
f2.l.[] = False
f2.l.k = if l==(take.(length.l).k) then True else f2.l.(tail.k) 


f3.[].[] = True 
f3.l.[] = False
f3.l.k = ( l==(take.(length.l).k) && True ) || f3.l.(tail.k)

