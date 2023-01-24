ctype Ar where
	E: Ar
	N: Int->Ar->Ar


ctype Tree where
	Et:Tree
	Ne:Int->Tree->Tree->Tree
	

ma.(N.x.E) = x
ma.(N.x.y) = max.x.(ma.y)

maxT.(Ne.x.Et.Et) = x
maxT.(Ne.x.y.Et)  = max.x.(maxT.y)
maxT.(Ne.x.Et.y)  = max.x.(maxT.y)
maxT.(Ne.x.y.z)   = max.x.(max.(maxT.y).(maxT.z))

sumT.Et           = 0
sumT.(Ne.x.Et.Et) = x
sumT.(Ne.x.y.Et)  = x + sumT.(y)
sumT.(Ne.x.Et.y)  = x + sumT.(y)
sumT.(Ne.x.y.z)   = x + sumT.(y) + sumT.(z)

levell.(Et) = 0
levell.(Ne.x.p.q) = (max.(levell.p).(levell.q))+1





