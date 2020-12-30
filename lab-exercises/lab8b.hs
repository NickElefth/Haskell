-- The goal of the second exercise is to build an interpreter for the simplest functional language (the
-- λ-calculus), using Haskell. 
-- λ-calculus is given by the following syntax -> M ::= var|(λx.M)|(MN)

-- All programs that we evaluate
-- will be closed terms (no free variables), and we will reduce to weak head normal form (we do not reduce under abstractions).

-- Reduction is given by (λx.t)u → t{x → u}, where t{x → u}

type Name = [Char]
data Lam = Var Name | Abs Name Lam | App Lam Lam deriving (Show)

type Set = [Name]

add x [] = [x]
add x (h:t) = if h == x then h:t else h:add x t

join [] x = x
join (h:t) x = join t (add h x) 

remove x [] = []
remove x (h:t) = if x == h then t else h:(remove x t) 

-- free variables of lambda
fv :: Lam -> Set
fv (Var s) = [s]
fv (App t u) = join (fv t) (fv u) 
fv (Abs x t) = remove x (fv t) 

-- check if closed term (no free variables)
closed :: Lam -> Bool
closed t = (fv t) == []

-- substitution of Lamda terms
subst:: Name -> Lam -> Lam -> Lam
subst v (Var v1) t = if v == v1 then t else Var v1 
subst v (Abs x l) t = if v == x then (Abs x l) else Abs x (subst v l t)
subst v (App e1 e2) t = App (subst v e1 t) (subst v e2 t)

-- weak head normal form reduction
eval (App t1 t2) = 
    case eval t1 of
        (Abs a u) -> eval (subst a u t2)
        t-> t
eval t = t

-- λx
i = Abs "x" (Var "x")

-- λfλx.f(fx)
two = Abs "f" (Abs "x" (App (Var "f") (App (Var "f") (Var "x"))))

-- λχ.χχ 
d = Abs "x" (App (Var "x") (Var "x"))