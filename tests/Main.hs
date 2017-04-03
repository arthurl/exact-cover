-- |

module Main where

import Test.Tasty (defaultMain, testGroup)

import qualified Math.ExactCover.Tests


main :: IO ()
main = defaultMain $ testGroup "Tests"
    [ Math.ExactCover.Tests.tests
    ]
