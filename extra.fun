
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

not


\x -> 0 - x
\x -> 0 + x
\a -> let b = a in b
\f -> f 1
\f -> \g -> \x -> f (g x)
\f -> \x -> f x #app
\f -> let res = f 1 in res
\f -> let res = \x -> f x in res
\f g x -> f (g x) #compose
\f x -> f (f x) #twice
\f x -> f x #app (multi-lam)
\x -> x
\z -> 1
\z -> \x -> x
\z -> let dub = \a -> \f -> f a a in dub
\x -> x

\a -> \b -> \c -> \d -> a (b c) (b c) # was bug: occurs check fail

# Blow up...
let f0 x = (x,x) in let f1 x = f0(f0 x) in let f2 x = f1(f1 x) in f2
let f0 k a b = k a b in let f1 k = k f0 f0 in let f2 k = k f1 f1 in f2


# More tests from previous times...

# application

(\a -> a) 42
(\a -> \b -> (b, a)) 1 2
(\a b -> (b, a)) 1 2
(\x -> (\y -> (y,y), \z -> (z,z))) 1
(\f g x -> f (g x)) (\x -> x)

# let
let inc = \x -> x + 1 in inc (inc 42)
let inc = \x -> x + 1 in inc 5
let x = 5 in x+x
let f x = x+x in f
let x = 42 in x
let xxx = 1 in let yyy = 2 in (xxx, yyy)
let f x = (x, x) in f 42
let f x y = (y, x) in f 1 2

# shadowing
let x = 3 in let x = true in x
let x = 3 in let y = x in let x = true in (x,y)
let x = 3 in \x -> x

# tuples
(1,true)
(1,2)
\a b -> (a,b)
\a b -> (a,b,1,a)
\a b -> (a,(b,1),a)
(\a -> a+1) 5
(10,10)
(10, true, ())
(((1,2),3),(4,5))
(1 * 2, 3 * 4)
