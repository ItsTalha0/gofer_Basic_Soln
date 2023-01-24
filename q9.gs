floatArr.l = map.(primIntToFloat).l				--Conversion of int array to float
sub1.[].[] = []							--sub function subtracts the 
sub1.l.[]  = l
sub1.(l).(k) = head.l - head.k :: sub1.(tail.l).(tail.k)
sub2.l.k =tail.(sub1.(l).(map.((*).(findD.l.k)).k))
findD.l.k = (head.l)/(head.k)
di.l.k.q = if length.l >= length.k then di.(sub2.(l).(k)).k.(q++[(findD.l.k)]) else (q,k)b
polyD.l.k = di.(floatArr.l).(floatArr.k).[]
sub2T.l.k = sub2.(floatArr.l).(floatArr.k)
