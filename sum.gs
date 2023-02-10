su.0 = [[]]
su.n = concat.[ addn.(su.(n-i)).i | i<-[1...n] ]
addn.l.i = [ p++[i] | p<-l ]
