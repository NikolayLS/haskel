

{--
  СУ "Св. Климент Охридски"
  Факултет по математика и информатика
  Курс Функционално програмиране 2020/21
  Контролно 3
  2021-01-16

  Име:Николай Любомиров Стефанов
  ФН:45545
  Специалност:Информатика
  Курс:3
  Административна група:1
  Начален час на контролното: 9;45
--}

module K3_45545 where
import Data.List
-- Това е примерна функция. Можете да напишете решението си на нейното място.
sample x = x


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




-------------------2ра задача

-- намирам интервала
interval a b = [x | x<- [a..b]]

--"мапвам" функцията на целия интервал
mapFunction f llist = [ (f x) | x<- llist ] 

--ако мапна функцията на интевала получавам списък - е ако този списък е съставен само от уникални елементи то фунцията е инекция (защото знаем че в интервала има само уникални)
isInjective f a b = (length (mapFunction f (interval a b))) == (length (nub(mapFunction f (interval a b))))
