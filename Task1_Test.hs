module DemoTest where

import Test.HUnit
import Task1


testTree = (Node 1 
                    (Node 2 
                            (Node 4 EmptyTree EmptyTree )
                             EmptyTree ) 
                    (Node 3 
                            (Node 5 EmptyTree EmptyTree ) 
                            EmptyTree ) )


testInorder = TestCase (assertEqual "Correct" (values Inorder (Node 5 (Node 3 EmptyTree EmptyTree ) (Node 6 EmptyTree EmptyTree )))  [3,5,6] )
testInorder2 = TestCase (assertEqual "Correct" (values Inorder (Node 3 (Node 5 EmptyTree EmptyTree ) (Node 8 EmptyTree EmptyTree )))  [5,3,8] )
testInorder3 = TestCase (assertEqual "Correct" (values Inorder testTree)  [4,2,1,5,3] )

testPostorder = TestCase (assertEqual "Correct" (values Postorder  (Node 5 (Node 3 EmptyTree EmptyTree ) (Node 6 EmptyTree EmptyTree )))  [3,6,5] )
testPostorder2 = TestCase (assertEqual "Correct" (values Postorder  (Node 3 (Node 5 EmptyTree EmptyTree ) (Node 8 EmptyTree EmptyTree )))  [5,8,3] )
testPostorder3 = TestCase (assertEqual "Correct" (values Postorder testTree)  [4,2,5,3,1] )


testPreorder= TestCase (assertEqual "Correct" (values Preorder  (Node 3 (Node 5 EmptyTree EmptyTree ) (Node 8 EmptyTree EmptyTree )))  [3,5,8] )
testPreorder2= TestCase (assertEqual "Correct" (values Preorder  (Node 5 (Node 3 EmptyTree EmptyTree ) (Node 6 EmptyTree EmptyTree )))  [5,3,6] )
testPreorder3 = TestCase (assertEqual "Correct" (values Preorder testTree)  [1,2,4,3,5] )

tests = TestList [testInorder ,testInorder2,testPostorder,testPostorder2,testPreorder,testPreorder2,testInorder3,testPostorder3,testPreorder3]
main = runTestTT tests
