ctype Nat where
	Z:Nat
	S:Nat->Nat
	P:Nat->Nat
	Inf:Nat

nplus.(Z).Z 		= Z
nplus.(S.x).Z 		= S.x
nplus.(Z).(S.x) 	= S.x
nplus.(S.x).(S.y) 	= S.(S.(nplus.(x).(y)))

nminus.(Z).Z		= Z
nminus.(S.x).(S.Z)	= x
nminus.(S.x).(Z)	= S.x

nsub.x.y 		= nplus.x.(convert.y)
convert.Z		= Z
convert.(S.x)		= P.(convert.x)
convert.(P.x)		= S.(convert.x)

isGreater.Z.Z		= False
isGreater.Z.(S.x)	= False
isGreater.(S.x).Z	= True
isGreater.(S.x).(S.y)	= True && isGreater.(x).(y)
