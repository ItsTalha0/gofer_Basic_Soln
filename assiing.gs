ls.(x::y::z).l = if y>x then ls.(y::z).(l++[x]) else l::ls.(y::z).([x])
ls.[].l = [l]

