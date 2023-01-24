ctoi.x = ord.x - ord.'0'
stoi.[x] = (ctoi.(x))
stoi.[] = 0
stoi.l = ctoi.(last.l) + stoi.(init.l) * 10


----------Expression Ctypes----------------------
ctype Op where
	Plus,Minus,Add,Sub,Mult,Div:Op

isOp.('+') 	= Plus
isOp.('-') 	= Sub
isOp.('*')	= Mult
isOp.('/')	= Div



ctype Exp where
	E:Op->Exp->Exp->Exp
	V:String->Exp
	Br:Exp->Exp
--seval.(E.Plus.(V.x).(V.y))	= Ev.(stoi.x + stoi.y)
--seval.(E.Plus.(Ev.x).(V.y))	= Ev.(x + stoi.y)
--seval.(E.Sub.(V.x).(V.y))	= Ev.(stoi.x - stoi.y)
--seval.(E.Mult.(V.x).(V.y))	= Ev.(stoi.x * stoi.y)

sEval.(V.x) 			= primIntToFloat.(stoi.x) 
sEval.(Br.x) 			= sEval.x
sEval.(E.Plus.x.y)		= (+).(sEval.x).(sEval.y)
sEval.(E.Sub.x.y)		= (-).(sEval.x).(sEval.y)
sEval.(E.Mult.x.y)		= (*).(sEval.x).(sEval.y)
sEval.(E.Div.x.y)		= (/).(sEval.x).(sEval.y)

sParse.[].a			= V.a
sParse.('*'::xs).a		= E.Mult.(V.a).(sParse.xs.[])
sParse.('/'::xs).a		= E.Div.(V.a).(sParse.xs.[])
sParse.('-'::xs).a		= E.Sub.(V.a).(sParse.xs.[])
sParse.('+'::xs).a		= E.Plus.(V.a).(sParse.xs.[])
sParse.(x::xs).a 		= sParse.(xs).(a++[x])

lParse:[Char]->[Char]->Exp    --Parses the given string without brackets and gives the expression to evaluator.
lParse.[].a = V.a
lParse.(l).a = if last.l /= '-'  && last.l /= '*' && last.l /='+' && last.l /= '/'   then lParse.(init.l).(last.l::a) else E.(isOp.(last.l)).(lParse.(init.l).[]).(V.a)




rBparse:[Char]->Int->[Char]   --gives the content after bracket
rBparse.l.0 = l
rBparse.l.c = if last.l == '(' || last.l ==')' then if last.l == ')' then rBparse.(init.l).(c+1)
								     else rBparse.(init.l).(c-1)
					       else rBparse.(init.l).c


sbParse:[Char]->Int->[Char]->[Char] -- Gives the content inside the bracket
sbParse.l.0.c = tail.c
sbParse.l.c.s = if last.l == '(' || last.l ==')' then if last.l==')' then sbParse.(init.l).(c+1).(last.l::s)
								    else sbParse.(init.l).(c-1).(last.l::s)
						 else sbParse.(init.l).c.(last.l :: s)


sBparse:[Char]->[Char]->Exp
sBparse.[].s = V.s
sBparse.l.s = if last.l /= '*' && last.l/= '+' && last.l /= '-' && last.l /= '/' && last.l /= ')'  then sBparse.(init.l).(last.l :: s)
	      else if last.(init.l) == ')' then E.(isOp.(last.l)).(sBparse.(rBparse.(init.(init.l)).1).[]).(sBparse.(sbParse.(init.(init.l)).1.[]).[])
	           else E.(isOp.(last.l)).(sBparse.(init.l).[]).(V.s)









--old Bracket extracktor.
oldBp:[Char]->Int->[Char]->[Char]
oldBp.([')']).0.s = s
oldBp.(')'::xs).0.s = s
oldBp.('('::xs).c.s = oldBp.xs.(c+1).(s++['('])
oldBp.(')'::xs).c.s = oldBp.xs.(c-1).(s++[')'])
oldBp.(x::xs).c.s   = oldBp.xs.c.(s++[x])



