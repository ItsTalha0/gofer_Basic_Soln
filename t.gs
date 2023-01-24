-- General programs
-- housekeeping and genral programs 


-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
aadd.(0).b= b
aadd.a.b = aadd.(a-1).(b+1)

size.a = length.a

--Multiply
mul.(0).b = b
mul.a.b = mul.(a-1).(aadd.b.b)

----1 third element of list
thirdele . (x::y::z::xs) = z

--2 second last element of a list
--secL : [a] -> a
--secL.(x::y::xs)= if size.xs == 0 then x else secL.(y::xs)

--3 sum upto integer n from 1
sumupto : Int -> Int
sumupto.0 = 0
sumupto.n = n+sumupto.(n-1)

--4 add 1st ele of tow lists
addFirst: [Int]->[Int]->Int
addFirst.[].[]=0
addFirst.(x::xs).(y::ys)= x+y

--5 table of n up to m elements
tableof: Int->Int->[Int]
tableof.n.0=[]
tableof.n.m=tableof.n.(m-1)++[n*m]

--6 number of ele bigger than n in a list
hbt : [Int]->Int->Int
hbt.(x::xs).n= if x>n then 1+hbt.xs.n else 0+hbt.xs.n
hbt.[].n=0

--7 sum all odd elements of a list
addOdd:[Int]->Int
addOdd.(x::xs)=if (rem.x.2)==1 then x+addOdd.xs else 0+addOdd.xs
addOdd.[]=0

--8 sum all primes up to n
sumprime:Int->Int
sumprime.0=0
sumprime.n = if isprime.n==True then n+sumprime.(n-1) else sumprime.(n-1)

--9 remove lists with odd no. elements
rmoddlist:[[a]]->[[a]]
rmoddlist.(x::xs) = if rem.(size.x).2==1 then rmoddlist.xs else x::rmoddlist.xs
rmoddlist.[]=[]

--10 compares two lists
comparelist:[Int]->[Int]->[Int]
comparelist.(x::xs).(y::ys)=if x>y then 1:: comparelist.xs.ys else 0::comparelist.xs.ys
comparelist.[].[]=[]

--11 reverse the list and add one to each ele.
reverseaddone:[Int] ->[Int]
reverseaddone.(x::xs) = reverseaddone.(xs) ++[x+1]
reverseaddone.[]=[]

--12
func:Int->Int
func.n = n*2+3

--13 smallest ele of list
smallest:[Int]->Int
smallest.[x]=x
smallest.(x::xs)= if x<smallest.xs then x else smallest.xs

--14 largest ele of list
largest : [Int] ->Int
largest.[]=0
largest . [x] = x
largest . (x::xs) = if x > (largest .xs) then x else (largest . xs)

--15 remove given ele from given list.
mrm : [Int] -> Int -> [Int]
mrm.[].y=[]
mrm.(x::xs).y=if x==y then mrm.xs.y else x:: mrm.xs.y

--16 given a list of list make a list of sec largest ele of each list 
secLarL: [[Int]] -> [Int]
secLarL.[]=[]
secLarL.(x::xs)=largest.(mrm. x . (largest.x) ):: secLarL . xs

--17 return list of factors of given int.
factor : Int -> [Int]
factor . n = fh.n.1.(n==1)

fh : Int -> Int -> Bool -> [Int]
fh.n.i.False = if ((rem.n.i) == 0) then i ::fh.n.(i+1).(n==(i+1)) else fh.n.(i+1).(n==(i+1))
fh.n.i.True = [n]

--18 check if given no. is prime no.
isprime:Int->Bool 
isprime.0=False
isprime.1=False
isprime.n = if size.(factor.n)<=2 then True else False

--20 sum the ele of list
summ : [Int]->Int
summ.[] = 0
summ.(x::xs) = x + summ.xs

--21 sumreq.[a,b,c,....,z]=[a+b+c+...+z,...,x+y+z,y+z,z]
sumreq.(x::xs)=summ.(x::xs)::sumreq.(xs)
sumreq.[]=[]

--22 sum2req.[a,b,c,....,z]=[a,a+b,a+b+c,.....,a+b+c+...+z]  
sum2req:[Int]->[Int]
sum2req.[]=[]
sum2req.(x::s) = sum2req.s ++ [sum.(x::s)]


feo.(x::xs)=x
rps:[Int]->[Int]
rps.[]=[]
rps.[x]=[x]
rps.(x::xs)=x+feo.(rps.xs)::rps.xs

psum:[Int]->[Int]
psum.(l) = psumh.l.(summ.(l))
psumh:[Int]->Int->[Int]
psumh.[].k=[]
psumh.(x::xs).k= k:: psumh.xs.(k-x)

--24 remove last ele
removelast:[a]->[a]
removelast.[x]=[]
removelast.(x::xs)=x::removelast.(xs)

--27 insert end
insertend:[a]->a->[a]
insertend.[].n=[n]
insertend.(x::ys).n = x::insertend.ys.n 

--26 last ele of list
mytail:[a]->a
mytail.[x]=x
mytail.(x::xs)=mytail.xs

--23 join 2 lists
joinlist:[a]->[a]->[a]
joinlist.(l).(x::xs)= joinlist.(insertend.l.x).xs
joinlist.l.[]=l

--24 join 3 lists
jL:[a]->[a]->[a]
jL.[].y=y
jL.(x::xs).y = x::(jL.(xs).y)

--25 Mid Num


--Is greater than 
isG:Int->Int->Bool
isG.(0).b= False
isG.a.(0) = True
isG.a.b= isG.(a-1).(b-1) 

-- Greatest in list
gL:[Int]->Int
gL.[x] = x
gL.(x::xs)=if x>gL.(xs) then x else gL.(xs)

--Fibonacci

fiB:Int->[Int]
fiB.(0)=[0]
fiB.(1)=[1,0]
fiB.a = (head.(fiB.(a-1))+head.(fiB.(a-2)))::fiB.(a-1)

-- Factorial
fact:Int->Int
fact.0 = 1
fact.a = a*fact.(a-1)

-- Find factor 
fa:Int->[Int]
fa.x = far.x.x
far:Int->Int->[Int]
far.x.0 = []
far.x.y = if mod.x.y == 0 then y::far.x.(y-1) else far.x.(y-1)


-- Is prime

prim.x = pri.x.(x-1)

pri:Int->Int->Bool
pri.x.1 = True
pri.x.y = if mod.x.y == 0 then False  else pri.x.(y-1)

-- Prime Factors

priFa:Int->[Int]
priFa.x = fart.x.(x-1)
fart.x.1 = [1]
fart.x.y = if (rem.x.y == 0) && prim.y == True then y::fart.x.(y-1) else fart.x.(y-1)

-- Dummy 

isN.x = if x/=0 then True else False










