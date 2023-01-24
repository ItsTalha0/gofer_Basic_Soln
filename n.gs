pun:[Int]->Bool
pun.(0::xs) = True
pun.(x::xs)=pun.(xs)
pun.[] = False

