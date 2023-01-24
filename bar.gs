nbar.0 = []
nbar.n = [(chr.219)]::nbar.(n-1)

addbar.(x::xs) = (x,nbar.x)::addbar.xs
addbar.[] = []

