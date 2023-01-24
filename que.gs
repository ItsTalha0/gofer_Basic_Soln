
--Question 9
polydiv.k.l = di.k.l.[] 
sub.[].[] = []
sub.(l).(k) = head.k - head.l :: sub.(tail.k).(tail.l)

--di.k.l.q = if length.k >= length.l then di.(tail.(sub.(take.(length.l).(map.((*).(findD.k.l).k))).(map.((*).(findD.l.k)).l))++drop.((length.l).(map.((*).(findD.k.l).k)))).l.(q++[rem.(lcm.(head.k).(head.l)).(head.l)]) else (q,k)


findD.k.l = rem.(lcm.(head.k).(head.l)).(head.k)

--sl.i.j = if i>0 and 
