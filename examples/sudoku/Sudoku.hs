-- |

module Sudoku
  (
    -- * Types
    Digit(..), allDigits
  , SRow(..), allRows
  , SCol(..), allColumns
  , SBox(..), box
  , SConstraint(..)

    -- * Map Sudoku to exact-cover problem
  , toConstraints
  , convert2SolvedGrid

    -- * Reading Sudokus
  , readLineFormat
  , readPEulerGrid
  )
where

import Sudoku.Grid

import Control.Arrow (second)
import Safe (readMay, headMay)
import qualified Data.Vector.Generic as V (generate)
import Data.Map (Map)
import qualified Data.Map.Strict as Map (lookup, fromList)
import Data.Set (Set)
import qualified Data.Set as Set (fromList)


data Digit = D1 | D2 | D3 | D4 | D5 | D6 | D7 | D8 | D9
  deriving (Eq, Ord, Enum)

instance Show Digit where
  show D1 = "1"
  show D2 = "2"
  show D3 = "3"
  show D4 = "4"
  show D5 = "5"
  show D6 = "6"
  show D7 = "7"
  show D8 = "8"
  show D9 = "9"

instance Read Digit where
  readsPrec _ ('1':rest) = [(D1, rest)]
  readsPrec _ ('2':rest) = [(D2, rest)]
  readsPrec _ ('3':rest) = [(D3, rest)]
  readsPrec _ ('4':rest) = [(D4, rest)]
  readsPrec _ ('5':rest) = [(D5, rest)]
  readsPrec _ ('6':rest) = [(D6, rest)]
  readsPrec _ ('7':rest) = [(D7, rest)]
  readsPrec _ ('8':rest) = [(D8, rest)]
  readsPrec _ ('9':rest) = [(D9, rest)]
  readsPrec _ _ = []

allDigits :: [Digit]
allDigits = [toEnum x | x <- [0..8]]

data SRow = R0 | R1 | R2 | R3 | R4 | R5 | R6 | R7 | R8
  deriving (Show, Eq, Ord, Enum)

allRows :: [SRow]
allRows = [toEnum x | x <- [0..8]]

data SCol = C0 | C1 | C2 | C3 | C4 | C5 | C6 | C7 | C8
  deriving (Show, Eq, Ord, Enum)

allColumns :: [SCol]
allColumns = [toEnum x | x <- [0..8]]

data SBox = B0 | B1 | B2 | B3 | B4 | B5 | B6 | B7 | B8
  deriving (Show, Eq, Ord, Enum)

-- | Given the row and column, get the box.
box :: SRow -> SCol -> SBox
box (fromEnum -> r) (fromEnum -> c) =
  toEnum $ (r `div` 3) * 3 + (c `div` 3)

data SConstraint = RowColumn !SRow !SCol
                  | RowNumber !SRow !Digit
                  | ColumnNumber !SCol !Digit
                  | BoxNumber !SBox !Digit
  deriving (Show, Eq, Ord)

instance Enum SConstraint where
  fromEnum (RowColumn r c) = fromEnum r * 9 + fromEnum c
  fromEnum (RowNumber r n) = 81 + fromEnum r * 9 + fromEnum n
  fromEnum (ColumnNumber c n) = 162 + fromEnum c * 9 + fromEnum n
  fromEnum (BoxNumber b n) = 243 + fromEnum b * 9 + fromEnum n

  toEnum x = case second (`divMod` 9) $ x `divMod` 81 of
    (a, (b, c)) | a == 0 -> RowColumn (toEnum b) (toEnum c)
                | a == 1 -> RowNumber (toEnum b) (toEnum c)
                | a == 2 -> ColumnNumber (toEnum b) (toEnum c)
                | a == 3 -> BoxNumber (toEnum b) (toEnum c)
                | otherwise -> error "toEnum instance error."

-- | Maps the Sudoku problem to a set of constraints following
-- https://en.wikipedia.org/wiki/Exact_cover#Sudoku.
toConstraints :: Grid (Maybe Digit)
              -> Map (Set SConstraint) (SRow, SCol, Digit)
toConstraints (toListList -> g) =
  let expanded :: [(SRow, SCol, Maybe Digit)]
      expanded = concatMap (\(r, xs) -> zip3 (repeat r) allColumns xs)
                 . zip allRows $ g
  in Map.fromList $ concatMap f expanded
  where
    f :: (SRow, SCol, Maybe Digit)
      -> [((Set SConstraint), (SRow, SCol, Digit))]
    f (r, c, Just n) = [clueExist r c n]
    f (r, c, Nothing) = clueNotExist r c

    clueExist :: SRow -> SCol -> Digit
              -> ((Set SConstraint), (SRow, SCol, Digit))
    clueExist r c n = let constraint = Set.fromList [ RowColumn r c
                                                    , RowNumber r n
                                                    , ColumnNumber c n
                                                    , BoxNumber (box r c) n
                                                    ]
                      in (constraint, (r, c, n))

    clueNotExist :: SRow -> SCol
                 -> [((Set SConstraint), (SRow, SCol, Digit))]
    clueNotExist r c = map (clueExist r c) allDigits

convert2SolvedGrid :: [(SRow, SCol, Digit)] -> Grid Digit
convert2SolvedGrid xs =
  let m = Map.fromList . map (\(r,c,n) -> ((r,c),n)) $ xs
  in Grid $ V.generate 9 $ \r ->
            V.generate 9 $ \c ->
              case Map.lookup (toEnum r, toEnum c) m of
                Just n -> n
                Nothing -> error "convert2SolvedGrid error."

-- | Parse single line Sudoku format.
readLineFormat :: String -> Grid (Maybe Digit)
readLineFormat (headMay . lines -> Just str) | length str == 81 =
  fromListList . chunksOf 9 . map (\c -> readMay [c]) $ str
  where
  chunksOf :: Int -> [a] -> [[a]]
  chunksOf _ [] = [[]]
  chunksOf n xs@(_:_) = let (y, rest) = splitAt n xs
                        in y : chunksOf n rest
readLineFormat _ = error "Invalid Sudoku grid."

-- | Parse project Euler Sudoku format.
readPEulerGrid :: String -> Grid (Maybe Digit)
readPEulerGrid (lines -> strS) | length strS >= 9 =
  fromListList $ map readPEulerLine strS
  where
    readPEulerLine :: String -> [Maybe Digit]
    readPEulerLine (take 9 -> strLn) = map (\c -> readMay [c]) strLn
readPEulerGrid _ = error "Invalid Sudoku grid."
