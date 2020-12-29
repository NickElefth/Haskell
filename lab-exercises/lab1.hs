-- Square of x
square x = x*x

--  identity function
identity i = i

-- first one
firstone x y = x

-- second one
secondone x y = y

--  exercise to visualise the haskell evaluation
fortytwo x = 42
infinity = infinity + 1

-- apply function
apply f x = f x

-- apply twice function
twice f x = f (f x)


-- fibonacci pattern matching
fibonacci 0 = 1
fibonacci 1 = 1
fibonacci n = fibonacci(n-1) + fibonacci(n-2)

-- fibonacci if then else
fib1 n = if n<2 then 1 else fib1(n-1) + fib1(n-2)

-- fibonacci guarded equations
fib2 n
    | n<2 = 1
    | otherwise = fib2(n-1)+fib2(n-2)