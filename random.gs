-- A simple function to generato random no list.

f.x = x*x
q.x = mod.x.9

random1.n = [ q.(f.i) | i<-[1...n] ]
random2.seed.0 = []
random2.seed.n = q.(f.seed) :: random2.(f.seed).(n-1) 




