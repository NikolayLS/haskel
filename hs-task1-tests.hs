
module K3_45545_Test where

import Test.HUnit
import K3_45545  


task1Test1 = TestCase $ assertEqual "sample check" True (isStringinTree "abd" sampleTree)
task1Test2 = TestCase $ assertEqual  "sample check" True (isStringinTree "ab" sampleTree)
task1Test3 = TestCase $ assertEqual  "sample check" True (isStringinTree "a" sampleTree)

task2Test1 = TestCase $ assertEqual  "sample check" True (isInjective succ 5 100)
task2Test2 = TestCase $ assertEqual  "sample check" True (isInjective sample 5 100)
task2Test3 = TestCase $ assertEqual  "sample check" False (isInjective even 5 100)
task2Test4 = TestCase $ assertEqual  "sample check" False (isInjective (\a -> 1) 5 100)

tl1 = TestList [task1Test1,task1Test2,task1Test3,task2Test1,
                task2Test2,task2Test3,task2Test4]

main = do
  runTestTT tl1
