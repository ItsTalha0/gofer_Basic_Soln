ctype Nat where
	Z:Nat
	S:Nat->Nat
	P:Nat->Nat
	Inf:Nat

natadd.x.y=nfold.f.(natplus.x.y)

natplus.Z.Z=Z
natplus.Z.x=x
natplus.x.Z=x
natplus.(S.x).(P.y)=natplus.x.y
natplus.(P.x).(S.y)=natplus.x.y
natplus.(S.x).n=natplus.x.(S.n)
natplus.x.(S. n)=natplus.(S.x).n
natplus.x.(P.n)=natplus.(P.x).n
natplus.(P.x).n=natplus.x.(P.n)


nateval.x=nfold.f.x

natmul.Z.x=Z
natmul.x.Z=Z
natmul.x.(S.n)=natadd.x.(natmul.x.n)
natmul.x.(P.n)=natadd.(convert.x).(natmul.x.n)

soe.x.y=issmaller.(nateval.x).(nateval.y)

issmaller.Z.Z=True
issmaller.(S.Z).(P.Z)=True
issmaller.Z.(S.x)=False
issmaller.(S.x).Z=True
issmaller.(S.x).(S.y)=issmaller.x.y
issmaller.(P.x).(P.y)=issmaller.x.y
issmaller.Z.(P.y)=True
issmaller.(P.x).Z=False
issmaller.(S.x).(P.y)=True
issmaller.(P.x).(S.y)=False

natsub.x.y=natadd.x.(convert.y)
natdiv.x.y=natdiv1.(nateval.x).(nateval.y)
natrem.x.y=natrem1.(nateval.x).(nateval.y)

natdiv1.Z.Z=Inf
natdiv1.Z.x=Z
natdiv1.(S.x).(S.y)=if (soe.(S.x).(S.y)) then natplus.(S.Z).(natdiv1.(natsub.(S.x).(S.y)).(S.y)) else Z
natdiv1.(S.x).(P.y)=convert.(natdiv.(S.x).(convert.(P.y)))
natdiv1.(P.x).(S.y)=convert.(natdiv.(convert.(P.x)).(S.y))
natdiv1.(P.x).(P.y)=natdiv.(convert.(P.x)).(convert.(P.y))

natrem1.Z.Z=Inf
natrem1.Z.x=Z
natrem1.(S.x).(S.y)=if(soe.(S.x).(S.y)) then natrem1.(natsub.(S.x).(S.y)).(S.y) else S.x
natrem1.(S.x).(P.y)=convert.(natrem.(S.x).(convert.(P.y)))
natrem1.(P.x).(S.y)=convert.(natrem.(convert.(P.x)).(S.y))
natrem1.(P.x).(P.y)=natrem.(convert.(P.x)).(convert.(P.y))


convert.Z=Z
convert.(S.x)=P.(convert.x)
convert.(P.x)=S.(convert.x)

-- ------------------------

nfold.f.Z=Z
nfold.f.(S.x)=f.(S.Z).(nfold.f.x)
nfold.f.(P.x)=f.(P.Z).(nfold.f.x)

f.(S.Z).Z=S.Z
f.(S.Z).(S.Z)=S.(S.Z)
f.(S.Z).(S.x)=S.(f.(S.Z).x)
f.(S.Z).(P.x)=x
f.(P.Z).Z=P.Z
f.(P.Z).(P.Z)=P.(P.Z)
f.(P.Z).(S.x)=x
f.(P.Z).(P.x)=P.(f.(P.Z).x)

-- ---------------------------------

ntoi.Z=0
ntoi.(S.x)=ntoi.x + 1
ntoi.(P.x)=ntoi.x - 1

iton.0=Z
iton.x=if (x>0) then S.(iton.(x-1)) else P.(iton.(x+1))

-- ---------------------
iseven.x=iseven1.(nateval.x)

iseven1.Z=True
iseven1.(P.Z)=False
iseven1.(S.Z)=False
iseven1.(S.(S.x))=iseven1.x
iseven1.(P.(P.x))=iseven1.x
