remdup.[] = []
remdup.(x::xs) = x :: remdup.(filter.(\y->not.(x==y)).xs)

re.(l) = foldr.(\x->(filter.((/=).x));((::).x)).[].(l)
