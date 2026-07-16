# Basic literals
42
true
false

# Variables and identity
\x -> x
\f -> f
\x -> \y -> x
\x -> \y -> y

# Application
(\x -> x) 42
(\x -> x) true
(\f -> \x -> f x) (\y -> y) 42

# Let expressions
let x = 42 in x
let f = \x -> x in f
let id = \x -> x in id 42
let id = \x -> x in id true
let f = \x -> x in let g = \y -> y in f (g 42)

# Tuples
(42, true)
(true, false, 42)
(\x -> (x, x)) 42
let pair = \x -> \y -> (x, y) in pair 42 true

# Higher-order functions
\f -> \x -> f x
\f -> \x -> f (f x)
\f -> \g -> \x -> f (g x)
let twice = \f -> \x -> f (f x) in twice
let compose = \f -> \g -> \x -> f (g x) in compose

# Complex examples
let k = \x -> \y -> x in k
let s = \f -> \g -> \x -> f x (g x) in s
let y = \f -> (\x -> f (x x)) (\x -> f (x x)) in y

# Polymorphic examples (simplified for our basic implementation)
let id = \x -> x in (id, id)
let const = \x -> \y -> x in const
let flip = \f -> \x -> \y -> f y x in flip

# Error cases (should fail)
\x -> x x
(\x -> x x) (\x -> x x)

# Tuple unification
\f a b c -> (f (a,b), f (c,1))

# Mismatch
\f -> (f 1, f true)
\f -> (f 1, f (2,3))
\f -> (f (1,2), f (3,4,5))
\f a -> (f (a,a), f (a,a,a))
\x -> 1 x

# Generlization
\id -> (id 1, id true)                  # type error expected
let id x = x in (id 1, id true)         # LetGen will avoid error here
\f -> let id x = f x in (id 1, id true) # should still type error here

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

# if-then-else
if (true) then 1 else 2
\a -> if (true) then a else a
\a -> if (true) then a else 1
\a -> if (true) then a else not a
\a -> if (true) then (1,a,true) else (2,a,false)
if true then if true then 1 else 2 else 3
(if true then 1 else 2, if false then 1 else 2)

# errors
\x -> x x
\a -> if (true) then a else (a,a)
false + 22
11 + true
true && 123
true && not
let inc = \x -> x + 1 in inc true
let x = true in x+x
not (+)
not 42
(42+1) 77
(\a -> a+1) (5,6)
(\f -> f 1) (5,6,7)
(1,2) 3
if (10) then 11 else 12
if (true) then 13 else ()
if (true) then (1,2,true) else (2,true,true)
\a -> if (true) then (1,a) else (a,false)
