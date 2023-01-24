ctoi.x = ord.x - ord.'0'
stoi.[x] = (ctoi.(x))
stoi.[] = 0
stoi.l = ctoi.(last.l) + stoi.(init.l) * 10


itos.k =if k>= 10 then itos.(k/10) ++ [chr.((mod.k.10)+ord.'0')] else [chr.(k+ord.'0')]
----------Expression Ctypes----------------------


ctype Exp where
	E:[Exp]->Exp
	Plu,Min,Mul,Div,Pow,Mod:Exp
	V:String->Exp
	Vi:Int->Exp
	Br:[Exp]->Exp

pArse.l = filte.(parse.(l).[])

parse.[].a= [V.a]
parse.('+'::xs).a = V.a::Plu::parse.(xs).[]
parse.('-'::xs).a = V.a::Min::parse.(xs).[]
parse.('*'::xs).a = V.a::Mul::parse.(xs).[]
parse.('/'::xs).a = V.a::Div::parse.(xs).[]
parse.('%'::xs).a = V.a::Mod::parse.(xs).[]
parse.('^'::xs).a = V.a::Pow::parse.(xs).[]

parse.('('::xs).a = Br.((parse.(inBracket.xs).[]))::(parse.(aBracket.xs).[])
parse.(x::xs).a = parse.(xs).(a++[x])

ieval.(V.a).(V.b).ope = Vi.((op.ope).(stoi.a).(stoi.b))
ieval.(Vi.a).(V.b).ope = Vi.((op.ope).(a).(stoi.b))
ieval.(Vi.a).(Vi.b).ope= Vi.((op.ope).a.b)
ieval.(V.a).(Vi.b).ope = Vi.((op.ope).(stoi.a).b)


op.Plu = (+)
op.Min = (-)
op.Mul = (*)
op.Div = (/)
op.Pow = (^)
op.Mod = (mod)




finalEval.(Vi.x) = x



vEval.(V.x) = Vi.(stoi.x)

sEval.[Vi.x] = Vi.x
sEval.(Br.x::xs) = sEval.(sEval.x :: xs)
sEval.(Br.x::y::Br.z::xs) = sEval.((ieval.(sEval.x).(sEval.z).y)::xs)
sEval.(Br.x::y::z::xs) = sEval.((ieval.(sEval.x).(z).y)::xs)
sEval.(x::y::Br.z::xs) = sEval.((ieval.(x).(sEval.z).y)::xs)
sEval.(x::z::y::xs) = sEval.(ieval.x.y.z::xs)
--sEval.(x::Minus::y::xs)= sEval.(Vi.(sEval.x-sEval.y)::xs)
--sEval.(x::Mult::y::xs) = sEval.(Vi.(sEval.x*sEval.y)::xs)
--sEval.(x::Div::y::xs)  = sEval.(Vi.(sEval.x/sEval.y)::xs)


inBracket.(l) = oldBp.(l).0.[]
oldBp:[Char]->Int->[Char]->[Char]
oldBp.([')']).0.s = s
oldBp.(')'::xs).0.s = s
oldBp.('('::xs).c.s = oldBp.xs.(c+1).(s++['('])
oldBp.(')'::xs).c.s = oldBp.xs.(c-1).(s++[')'])
oldBp.(x::xs).c.s   = oldBp.xs.c.(s++[x])


aBracket.l = afterB.(l).0.[]
afterB.(')'::xs).0.s = xs
afterB.('('::xs).c.s = afterB.xs.(c+1).(s++['('])
afterB.(')'::xs).c.s = afterB.xs.(c-1).(s++[')'])
afterB.(x::xs).c.s   = afterB.xs.c.(s++[x])

filte.[] = []
filte.(V.[]::xs) = filte.(xs)
filte.(Br.x::xs) = Br.(filte.(x) ):: filte.(xs)
filte.(x::xs)    = x::filte.(xs)

--Bodmas eval using brackets

--Bracket evaluation
braEval.[Br.x] = bodEval.x
braEval.[x] = [x]
braEval.(Br.x::xs) = head.(bodEval.x) :: braEval.(xs)
braEval.(x::xs) = x::braEval.(xs)

--Power eval
pEval.[x] = [x]
pEval.(x::Pow::y::z) = pEval.(ieval.x.y.Pow::z)
pEval.(x::y::z::xs) = x::y::pEval.(z::xs)

--Division eval
dEval.[x] = [x]
dEval.(x::Div::y::z) = dEval.(ieval.x.y.Div::z)
dEval.(x::y::z::xs) = x::y::dEval.(z::xs)

--Multiplication Eval
mEval.[x] = [x]
mEval.(x::Mul::y::z) = mEval.(ieval.x.y.Mul::z)
mEval.(x::y::z::xs) = x::y::mEval.(z::xs)

--Subtraction eval
suEval.[x] = [x]
suEval.(x::Min::y::z) = suEval.(ieval.x.y.Min::z)
suEval.(x::y::z::xs) = x::y::suEval.(z::xs)

--Add eval
aEval.[x] = [x]
aEval.(x::Plu::y::z) = aEval.(ieval.x.y.Plu::z)
aEval.(x::y::z::xs) = x::y::aEval.(z::xs)

--ModEval
modEval.[x]= [x]
modEval.(x::Mod::z::xs) = modEval.(ieval.x.z.Mod::xs)
modEval.(x::y::z::xs) = x::y::modEval.(z::xs)

--Final bodmas eval with brackets.
bodEval.(l) = aEval.(suEval.(mEval.(dEval.(modEval.(pEval.(braEval.l))))))


compEval.l =show.(finalEval.(head.(bodEval.(pArse.l))))

noEmptyBrac.[] = True
noEmptyBrac.('('::')'::xs) = False
noEmptyBrac.(x::xs) = True && noEmptyBrac.(xs)
validInput.[] = True
validInput.(x::xs) =(x/=' ')&&( (ord.x<ord.'9' && ord.'0' < ord.x) ||  ( x=='+' || x=='-' || x=='*' || x=='/' || x=='%' || x=='^' || x=='(' || x==')' ) )&&validInput.(xs) 

complexEval.l = if validInput.l && noEmptyBrac.l then show.(finalEval.((sEval.(pArse.l)))) else "Invalid argument"
complexBodmasEval.l = if validInput.l && noEmptyBrac.l then compEval.l else "Invalid Input"

