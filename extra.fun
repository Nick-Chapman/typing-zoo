
# Blow up...
#let f0 x = (x,x) in let f1 x = f0(f0 x) in let f2 x = f1(f1 x) in f2
#let f0 k a b = k a b in let f1 k = k f0 f0 in let f2 k = k f1 f1 in f2

# Literals...
()
true
42
#42.7 # No floating point
"hello"
'x'

# Operators...

#- 14 # prefix
#+ 15 # prefix
1+2
3-1
3*2
6/2
#6 `div` 2
#6 `mod` 5
#5 `cmp` 5
#7/(-3)
#7/(+3)
true && not false
#- 2147483647
#-(+1)
16/4/2
16/(4/2)
16+4/2
true || true
not true
(+) 1 2
