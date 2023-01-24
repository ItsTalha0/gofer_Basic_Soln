--Tower of hanoi

mSolve.n = solve."s"."d"."t".n

solve.s.d.t.0 = []
solve.s.d.t.n = solve.s.t.d.(n-1)++[(n,s,d)]++solve.t.d.s.(n-1)

