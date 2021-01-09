
-- Factorial normal version
fact n = if n==0 then 1 else n*fact(n-1)

-- fact 3
-- 3*(fact 2)
-- 3*(2*(fact 1))
-- 3*(2*(1*(fact 0))
-- 3*(2*(1*1))
-- 3*(2*1)
-- 3*2
-- 6

factacc n acc = if n == 0 then acc else factacc (n-1) (acc*n)

-- factacc 3 1
-- factacc 2 3
-- factacc 1 6
-- factacc 0 6
-- 6

-- length 
len [] = 0
len (h:t) = 1+len(t)

lenacc [] acc = acc
lenacc (h:t) acc = lenacc t (acc+1)

--  power 
power(x,y) = if y==0 then 1 else x*power(x,y-1)

poweracc (x,y) acc = if y==0 then acc else poweracc (x,y-1) (acc*x)

-- positives 
positives [] = []
positives (h:t) = if h<0 then positives t else h:positives t

positivesacc [] acc = acc
positivesacc (h:t) acc = if h>=0 then positivesacc t (acc ++ [h]) else positivesacc t acc

-- product of array
product1 [] = 1
product1 (h:t) = h*product1(t)

productacc [] acc = acc
productacc (h:t) acc = productacc t (acc*h)