-- Ex 1 ~ Types
-- p -> p
i x = x
-- p1 -> p2 -> p1
k x y = x
-- p1 -> p2 -> p2
zero f x = x
-- (t1 -> t2) -> t1 -> t2
one f x = f x
-- (t1 -> t2) -> t1 -> t2
two f x = f(f x)
-- (t1 -> t2) -> t1 -> t2
three f x = f(f(f x))
-- (t1 -> t2 -> t3) -> (t1 -> t2) -> t1 -> t3
s x y z = x z (y z)
-- (t1 -> t1 -> t2) -> t1 -> t2
w x y = x y y
-- no type
-- d x y = x x y

-- p -> p
newi = s k k
-- (Eq t1, Num t1, Num t2) => t1 -> t2 -> t2 -> t2
fib n x y = if n==1 then x else fib (n-1) (x+y) x
-- (Eq a, Num a, Num c) => (a, c, c) -> c
fib2 (n,x,y) = if n==1 then x else fib2 (n-1, x+y, x)


-- fix method 
fix f = f(fix f)

fact = \f -> \x -> if x==0 then 1 else x*f(x-1)