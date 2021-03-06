
module K3_45545 where
import Data.List

data Tree a = EmptyTree | Node {
                            value :: a,
                            left  :: Tree a,
                            right :: Tree a
                          } deriving (Show,Read)

sampleTree = Node 'a' (Node 'b' (Node 'd' EmptyTree
                                          (Node 'g' EmptyTree EmptyTree))
                                (Node 'e' EmptyTree EmptyTree))
                      (Node 'c' EmptyTree
                                (Node 'f' EmptyTree EmptyTree))

isStringinTree string EmptyTree = False
isStringinTree string (Node root l r)
  |((length string) == 1) && (root == (head string)) = True
  |null string = True
  |root==(head string) = ( (isStringinTree (tail string) l) || (isStringinTree (tail string) r))
  |otherwise = ((isStringinTree string l) || (isStringinTree string r))





-- намирам интервала
interval a b = [x | x<- [a..b]]

--"мапвам" функцията на целия интервал
mapFunction f llist = [ (f x) | x<- llist ] 

--ако мапна функцията на интевала получавам списък - е ако този списък е съставен само от уникални елементи то фунцията е инекция (защото знаем че в интервала има само уникални)
isInjective f a b = (length (mapFunction f (interval a b))) == (length (nub(mapFunction f (interval a b))))
