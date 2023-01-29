
------ Main zipFunc  func

huffmanzip.l = compress.l

genTree.l = getTree.l

padding.l = paddinCal.l


------ Main Unzip func

huffmanunzip.compressedString.l.bitPadding  = tTree.(finalP.compressedString.bitPadding).(getTree.l).(getTree.l)   --l is the uncompressed string.

-- Sample command to test unzip.
-----huffmanunzip.(compress."abcd")."abcd".(padding."abcd")



---++++++++++++++++++++++++++++++++++++++++++--------------------------------------------------------------------------------


-- Freq tab [('a',cont of a),('b',count of b),('c',count of c) ... ]
--tableOfa.(listAll.(treeBuild.(reverse.(sort.(countChar."aaabdcd".[]))))) 
ctype Huff where
	L:Char->Huff
	N:Int->Huff->Huff->Huff
	E:Int->Char->Huff
	Et:Huff

----------------- Simple Cmparator for the huffman tree type
huffCmp.(E.x.y).(E.b.a) = b>x
huffCmp.(N.x.y.z).(E.b.a) = b>x
huffCmp.(E.x.y).(N.b.a.z) = b>x
huffCmp.(N.x.y.z).(N.b.a.p) = b>x
-------------------Count adder
addCount.(N.x.a.b).(N.y.p.q) = x+y
addCount.(N.x.a.b).(E.y.p)   = x+y
addCount.(E.y.p).(N.x.a.b)   = x+y
addCount.(E.y.p).(E.x.a)     = x+y

findAdd.((x,y)::xs).p = if y==p then (x+1,y)::xs else (x,y)::findAdd.(xs).p
findAdd.[].p = [(1,p)]

countChar.[].a = a
countChar.(x::xs).a  = countChar.(xs).(findAdd.(a).x)

remSum.[] = 0
remSum.((x,y)::xs) = x + remSum.(xs)

aList.l  = map.(\(x,y)->E.x.y).l

insertAt.(a).[] = [a]
insertAt.(a).(x::xs) = if huffCmp.a.x then a::x::xs else x::insertAt.a.xs 



makeTree.a.b = N.(addCount.a.b).a.b 

treeBuilder.[x] = [x]
treeBuilder.(x::y::xs) = treeBuilder.(insertAt.(makeTree.x.y).(xs))


---------------------------------------------------------------------End of new tree Format

treeBuild.[(x,y),(p,q)] =if x<=p then  N.(x+p).(L.y).(L.q) else N.(x+p).(L.q).(L.y)
treeBuild.((x,y)::xs) =if x<=remSum.xs  then  N.((+).(x).(remSum.xs)).(L.y).(treeBuild.xs) else N.((+).(x).(remSum.xs)).(treeBuild.xs).(L.y)

addLtoAll.x.l = [ x::i | i<-l ]
listAll.(E.x.e) = [e::""]
listAll.(N.c.(l).(r)) =addLtoAll.'0'.(listAll.l) ++ addLtoAll.'1'.(listAll.r)

tableOfa.l = [ (last.i,init.i) | i<- (listAll.(head.(treeBuilder.(aList.(sort.(countChar.l.[])))))) ]

findPat.x.l = [ pat | (ch,pat) <- tableOfa.l , ch==x ]

makePat.[].l = []
makePat.(x::xs).l = findPat.x.l::  makePat.xs.l

ctoi.x = ord.x - ord.'0'

stoi.[].p = 0
stoi.l.p = ctoi.(last.l)*(2^p) + stoi.(init.l).(p+1)

binToassci.[] = []
binToassci.l = if length.l<7 then (stoi.(l++[ '0' | i<- [1...(7-length.l)]]).0)::[]    
			      else  (stoi.(take.7.l).0)::binToassci.(drop.7.l) 					--convert the bin pattern into final assci pattern

assciPat.l = concat.(concat.(makePat.l.l))     									-- Get Binary Pattern of tree.

paddinCal.l =(-).(7).(rem.(length.(concat.(concat.(makePat.l.l)))).7)  -- Calculate the padding required

chrCon.l = [ chr.i | i <- l ]
compress.l = chrCon.(binToassci.(concat.(concat.(makePat.l.l)))) -- Main compressor function

flat1.l =map.(\x->lrto01.(init.x)++show.(ord.(last.x))).(listAll.(head.(treeBuilder.(aList.(sort.(countChar.l.[]))))))  --Gives flat string version of tree.

getTree.l = head.(treeBuilder.(aList.(sort.(countChar.l.[]))))  --Just returns the tree for a given string

freqString.[] = []
freqString.((x,y)::xs) = (show.(ord.(y)) ++ ','::show.x) :: freqString.xs
fString.l =concat.([ i++[','] | i <- init.l ] ++ [last.l])


------------------------------- Decompression Part --------------------------------------------------------------------------------------
strToi.[]= 0
strToi.l = ctoi.(last.l)+10*(strToi.(init.l))


stringToarr.[].a      = [strToi.a]
stringToarr.(','::xs).a = strToi.a::stringToarr.(xs).[]
stringToarr.(x::xs).a = stringToarr.(xs).(x::a)


lrto01.l = map.(\x->if x=='0' then 'L' else 'R').l

itobc.0 = ['0']
itobc.1 = ['1']
itobc.i = itobc.(i/2) ++ [ chr.((rem.i.2)+ord.'0') ]

bcToa.l = if length.l < 7 then [  '0' | i<- [1...7-length.l]]++l  else l		-- Convert the pattern to 7 bits.

intTobin.l =concat.[ bcToa.(itobc.i) | i <-(map.(\x->ord.x).l) ]			-- Convert the compressed string to binary string

finalP.l.k = take.(length.(intTobin.l) - k).(intTobin.l) 				-- Remove padding

tTree.[].a.b = []									-- Tree traversal function that get backs the orignal compressed string
tTree.('0'::xs).(N.x.(E.i.c).r).t = c::tTree.(xs).t.t
tTree.('1'::xs).(N.x.l.(E.i.c)).t = c::tTree.(xs).t.t
tTree.('0'::xs).(N.x.l.r).t       = tTree.(xs).l.t
tTree.('1'::xs).(N.x.l.r).t	  = tTree.(xs).r.t





