module Main where

import Math.ExactCover
import Sudoku

import Control.Monad (forever)


main :: IO ()
main = do
  let f sudokuPuzzleStr =
        case solve . toConstraints . readLineFormat $ sudokuPuzzleStr of
          [] -> putStrLn "No solution."
          soln@(_:_) -> print . convert2SolvedGrid $ soln
  forever $ do
    putStrLn "Enter puzzle in single line format:"
    putStrLn "(e.g. 8..........36......7..9.2...5...7.......457.....1...3...1....68..85...1..9....4..)"
    getLine >>= f
