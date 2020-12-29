-- Sum all elements of a list
sumAll [] = 0
sumAll (h:t) = h + sumAll t

-- Multiply all elements of a list
multAll [] = 1
multAll (h:t) = h * multAll t

fold f s [] = s
fold f s (h:t) = f h (fold f s t)

-- length of the list
len list = fold (\x -> \y -> 1+y) 0 list

-- max element
maxElem [] = 0
maxElem (h:t) = fold (\x -> \y -> if x > y then x else y) h t

-- flatten a list
flatten = fold (++) []


iter p f x = if (p x) then x else iter p f (f x)

f n = snd(iter (\(x,y) -> x>n) (\(x,y) -> (x+1,x*y)) (1,1))
-- (1,1) -> (2,1) -> (3,2) -> (4,6) -> (5,24) -> (6,120)

---------------------------------------------------------------------------------------
-- Accumulating parameters 

fact n = if n==0 then 1 else n*fact(n-1)

factcps n k = if n==0 then k 1 else factcps (n-1) (\r -> k (n*r))

factacc n acc = if n==0 then acc else factacc (n-1) (n*acc)

len1 [] = 0
len1 (h:t) = 1+len1 t

lenAcc [] acc = acc
lenAcc (h:t) acc = lenAcc t (acc + 1)


rev [] = []
rev (h:t) = (rev t) ++ [h]

revacc [] acc = acc
revacc (h:t) acc = revacc t ([h] ++ acc)

-----------------------------------------------------------------------------------------------
data IntOrBool = I Int | B Bool deriving Show
-- [I 3,B True] allow booleans and int in a list

data Lam = Var [Char] | Abs [Char] Lam | App Lam Lam deriving (Show)
-- representation of lamda calculus
