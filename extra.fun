
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

