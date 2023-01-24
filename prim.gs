isPrime.x.2 = mod.(x).2 /= 0
isPrime.x.n = mod.(x).n /=0 && isPrime.x.(n-2)

prime.x = if rem.x.2 == 0 && x/=2 then False else isPrime.x.(x-1)



-- square root using newtons approximation method i.e xi = xi - f(xi)/f;(xi)

fx.x = x*x - x

ntroot.x.1 = x - (fx.x)/(2*x)
ntroot.x.n = ntroot.x.(n-1) - (fx.(ntroot.x.(n-1)))/(2*ntroot.x.(n-1) )
