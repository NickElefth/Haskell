-- IMPERATIVE LANGUAGE IMPLEMENTATION

type Name = [Char]
type Memory = [(Name, Integer)]

-- update memory location
update n v [] = [(n, v)]
update n v ((n1, v1): t) = if n == n1 then (n,v):t else (n1,v1):update n v t

-- Find in memory
find n [] = 0
find n ((n1, v1): t) = if n == n1 then v1 else find n t

-- Arithmetic Expressions
-- E ::= Number | Var | E + E | E * E
data Aexp = Num Integer
        | Var Name
        | Add Aexp Aexp
        | Mult Aexp Aexp
        deriving (Show)

evalA :: Aexp -> Memory -> Integer
evalA (Num n) s = n
evalA (Var v) s = find v s
evalA (Add a b) s = (evalA a s) + (evalA b s)
evalA (Mult a b) s = (evalA a s) * (evalA b s)

-- Boolean Expressions
-- B ::= B and B | E = E | E < E | not B
data Bexp = And Bexp Bexp
            | Equal Aexp Aexp
            | LessThan Aexp Aexp
            | Not Bexp
            deriving (Show)

evalB (Not b) s = not(evalB b s)
evalB (Equal a b) s = (evalA a s) == (evalA b s) 
evalB (LessThan a b) s = (evalA a s) < (evalA b s) 
evalB (And a b) s = (evalB a s) && (evalB b s) 

-- Commands
-- C ::= Var = A | (C ; C) | If B then C else C | While B do C
data Command = Assign Name Aexp 
            | If Bexp Command Command
            | While Bexp Command
            | Seq Command Command 
            deriving (Show)

evalC (Assign n e) s = update n (evalA e s) s
evalC (Seq c1 c2) s =  evalC c2 (evalC c1 s)
evalC (If b c1 c2) s = if (evalB b s) then evalC c1 s else evalC c2 s
evalC (While b c1) s = if evalB b s then evalC(While b c1) (evalC c1 s) else s

-- x=1 ; y = z; x=z
p1 = Seq (Assign "x" (Num 1)) (Seq (Assign "y" (Var "x")) (Assign "x" (Var "z")))

-- z = 5 ; x = 4 ; if z < x then y = z else y = x
p2 = Seq (Assign "z" (Num 5)) 
    (Seq (Assign "x" (Num 4) ) 
    (If (LessThan (Var "z") (Var "x"))
        (Assign "y" (Var "z")) (Assign "y" (Var "x"))))

-- z = 5 ; x = 3 ; while x < z do (y=y+1 ; x=x+1)
p3 = Seq (Assign "z" (Num 5)) 
    (Seq (Assign "x" (Num 4) ) 
        (While (LessThan (Var "x") (Var "z")) 
        (Seq (Assign "y" (Add (Var "y") (Num 1)) )
                (Assign "x" (Add (Var "x") (Num 1))))))

-- y =1; z = 5 ; x = 1 ; while not(z < x) do (y=y*x ; x=x+1)
p4 = Seq (Assign "y" (Num 1))
        (Seq (Assign "z" (Num 5))
            (Seq (Assign "x" (Num 1))
                (While (Not (LessThan (Var "z") (Var "x")))
                    (Seq (Assign "y" (Mult (Var "y") (Var "x")) )
                        (Assign "x" (Add (Var "x") (Num 1)) )
                    )
                )
            )
        )