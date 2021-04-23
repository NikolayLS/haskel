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


module K3_45545_Test where

import Test.HUnit
import K3_45545  

-- Даденият по-долу пример е само ориентировъчен.
-- Когато решавате задачите си, можете да го изтриете
-- или да го промените така, че да проверява условия
-- свързани с вашия код.

-- Може да използвате пълния синтаксис...

--тестове по първа задача
task1Test1 = TestCase $ assertEqual "sample check" True (isStringinTree "abd" sampleTree)
task1Test2 = TestCase $ assertEqual  "sample check" True (isStringinTree "ab" sampleTree)
task1Test3 = TestCase $ assertEqual  "sample check" True (isStringinTree "a" sampleTree)

--тестове по втора задача
task2Test1 = TestCase $ assertEqual  "sample check" True (isInjective succ 5 100)
task2Test2 = TestCase $ assertEqual  "sample check" True (isInjective sample 5 100)
task2Test3 = TestCase $ assertEqual  "sample check" False (isInjective even 5 100)
task2Test4 = TestCase $ assertEqual  "sample check" False (isInjective (\a -> 1) 5 100)

tl1 = TestList [task1Test1,task1Test2,task1Test3,task2Test1,
                task2Test2,task2Test3,task2Test4]

main = do
  runTestTT tl1