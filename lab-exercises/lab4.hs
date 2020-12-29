-- data types
data List = Nil | Cons Int List deriving Show

len Nil = 0
len (Cons _ t) = 1 + len t

-- define my boolean data type
data MyBool = T | F deriving Show

myand T T = T
myand _ _ = F

myor F F = F
myor _ _ = T 

mynot T = F
mynot F = T 

-- Rock Paper Scissors 
data RPS = Rock | Paper | Scissors deriving Show

beats Scissors Paper = True
beats Paper Rock = True
beats Rock Scissors = True
beats _ _ = False

data Nat = Zero | Suc Nat deriving Show

-- addition
add Zero y = y
add (Suc x) y = Suc(add x y)
-- tail recursive
-- add (Suc x) y = add x (Suc y)

-- multiplication
mult Zero y = Zero
mult (Suc x) y = add(mult x y) y
-- (x+1)*y = x*y + y

-- check if equal 
equal Zero Zero = True
equal (Suc x) (Suc y) = equal x y
equal _ _ = False

