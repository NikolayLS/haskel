module Task1 where

data Tree a = EmptyTree | Node {
                            value :: a,
                            left  :: Tree a,
                            right :: Tree a
                          } deriving (Show,Read)


getValue :: Tree a -> a
getValue (Node v l r)= v
getLeft :: Tree a -> Tree a
getLeft (Node v l r)= l
getRight :: Tree a -> Tree a
getRight(Node v l r)= r


---------------------------------------------
inorder :: Tree a -> [a]
inorder EmptyTree = [] 
inorder t  = (inorder (getLeft t)) ++ [getValue t] ++ (inorder (getRight t))

preorder :: Tree a -> [a]
preorder EmptyTree = [] 
preorder t  = [getValue t] ++ (preorder (getLeft t)) ++ (preorder (getRight t))

postorder  :: Tree a -> [a]
postorder  EmptyTree = [] 
postorder  t  = (postorder (getLeft t)) ++ (postorder (getRight t)) ++ [getValue t]

-------------------------------------------------



data Strategy = Inorder | Postorder | Preorder deriving (Show,Read)


values :: Strategy -> (Tree a) -> [a]
values Inorder t = inorder t
values Preorder t = preorder t
values Postorder t = postorder t