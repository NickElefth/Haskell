
-- list equality check

equal [] [] = True
equal [] x = False
equal x [] = False
equal (x:xs) (h:t) = if x == h then equal xs t else False

-- reverse a list
rev [] = []
rev (h:t) = rev t ++ [h]

-- check if list is a palindrome
isPalindrome [] = True
isPalindrome list = equal list (rev list)


data BinaryTree a = EmptyTree | Node a (BinaryTree a) (BinaryTree a)  deriving (Show)


-- write a map function that applies a function to all nodes of the tree
mapTree f EmptyTree = EmptyTree
mapTree f (Node v l r) = Node (f v) (mapTree f l) (mapTree f r)