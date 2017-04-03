-- |

module Math.ExactCover.Tests where

import Math.ExactCover

import Test.Tasty (TestTree, testGroup)
import qualified Test.Tasty.HUnit as U
import Test.Tasty.HUnit ((@?=))

import qualified Data.Map.Strict as Map (fromList)
import qualified Data.Set as Set (fromList)


tests :: TestTree
tests = testGroup "Math.ExactCover"
  [ tests_solve
  ]


tests_solve :: TestTree
tests_solve = testGroup "solve"
  [ U.testCase "regular test case" case_solve_reg1
  ]

case_solve_reg1 :: U.Assertion
case_solve_reg1 =
  ( Set.fromList . solve $ Map.fromList [ (Set.fromList [2,4,5::Int], 'A')
                                        , (Set.fromList [0,3,6], 'B')
                                        , (Set.fromList [1,2,5], 'C')
                                        , (Set.fromList [0,3], 'D')
                                        , (Set.fromList [1,6], 'E')
                                        , (Set.fromList [3,4,6], 'F')
                                        ]
  ) @?= Set.fromList "ADE"
