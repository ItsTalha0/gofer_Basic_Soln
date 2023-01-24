bs:[Int]->[Int]->[Int]
bs.ls.(x::xs) = bs.(sw.ls).xs
bs.ls.[] = ls


sw:[Int]->[Int]

sw.[] = []
sw.[x]= [x]
sw.(x::y::z) = if x>y then y::sw.(x::z) else x::sw.(y::z)



bubbleSort:[Int]->[Int]
bubbleSort.ls = bs.ls.ls


