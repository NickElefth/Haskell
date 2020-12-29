-- Types of the following 
f x y = x + y -- Num a => a -> a -> a
g (x, y) = x + y -- Num a => (a, a) -> a

--  sum of 3 integer values with mixing parameters passing
add3a x y z = x + y + z
add3b (x, y) z = x + y + z
add3c (x, y, z) = x + y + z

--  function that takes four arguments us 1st arg once 2nd twice third once and ignore the fourth 
fourargs a b c d = a + b + b + c  -- Num a => a -> a -> a -> p -> a

-- curry 
-- curry g 3 4

-- -- uncurry
-- uncurry f (3, 4)

mycurry f x y = f (x,y)

myuncurry f (x, y) = f x y


----------------------------------------------------------------------------------------------------------------

-- test if list in ascending order
inorder [] = True
inorder [x] = True
inorder(x:y:xs) = if x < y then inorder xs else False

-- insert in sorted list
insert x [] = [x]
insert x (h:t) = if x < h then [x] ++ (h:t) else h:(insert x t)

-- sort 
sort [] = []
sort (h:t) = insert h (sort t)

----------------------------------------------------------------------------------------------------------------

-- return first elements of list
first n (h:t) = if n==0 then [] else h:first (n-1) t

-- filter out multiples from a list
filt x [] = []
filt x (h:t) = if mod h x == 0 then filt x t else h:filt x t

--  generate primes
primes (h:t) = h:primes(filt h t)
