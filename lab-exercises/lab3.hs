-- add one to all elements of the list
addOneToAll [] = []
addOneToAll (h:t) = (h+1):addOneToAll t

-- list comprehension
addOneToAllLC l = [x+1 | x<-l]

-- times two to all elements of the list
timesTwoToAll [] = []
timesTwoToAll (h:t) = (h*2):timesTwoToAll t

-- list comprehension
timesTwoToAllLC l = [x*2 | x<-l]

addOne x = x + 1 
timesTwo x = x * 2  

mymap f [] = []
mymap f (h:t) = (f h): mymap f t 

-- convert booleans to binary using map
-- map (\x -> if x then 1 else 0) [True,False,True,True]

-- convert a list of lists into a list returning only the first element of the list
-- map (\(x:t) -> x) [[1,2,3], [2,3,4], [3,4,5]] 

-------------------------------------------------------------------------------
-- TREES
data BinaryTree a = EmptyTree | Node a (BinaryTree a) (BinaryTree a)  deriving (Show)

-- convert binary tree to in order list Node v l r
-- example toList(Node 3 (Node 4 EmptyTree EmptyTree) EmptyTree) --> [4,3]
toList EmptyTree = []
toList (Node v l r) = (toList l) ++ [v] ++ (toList r)

isSorted [] = True
isSorted [x] = True
isSorted(x:y:xs) = if x < y then isSorted xs else False

-- check if tree is sorted
isTreeSorted (Node v l r) = isSorted (toList(Node v l r))


-- insert into tree
insertTree x EmptyTree = Node x EmptyTree EmptyTree
insertTree x (Node v l r)
    | x<=v = Node v (insertTree x l) r
    | otherwise = Node v l (insertTree x r)

-- preorder list of tree
preorder EmptyTree = []
preorder (Node v l r) = [v] ++ preorder l ++ preorder r

-- postorder list of tree
postorder EmptyTree = []
postorder (Node v l r) =  postorder l ++ postorder r ++ [v]

-- reverse a list
revList [] = []
revList (h:t) = revList t ++ [h]

-- revTree 
revTree EmptyTree = EmptyTree
revTree(Node v l r) = Node v (revTree r) (revTree l)