num2bin.0="0"

num2bin.n = increament.(num2bin.(n-1))
increament.[] = "1"
increament.x =if last.x == '0' then init.x ++ "1" else increament.(init.x)++"0"

