-- |

module Sudoku.Grid
  (
    -- * Types
    Grid(..)

    -- * Conversion
  , fromListList
  , toListList

    -- * Printing
  , prettyPrintGrid

    -- * Access
  , gridElem

  )
where

import Data.Functor.Identity (Identity(..))
import Data.List (transpose)
import qualified Data.Vector as VB (Vector)
import qualified Data.Vector.Generic as V (fromList, toList)
import Data.Vector.Generic ((!))
import qualified Text.PrettyPrint.Boxes as Box


newtype Grid a = Grid (VB.Vector (VB.Vector a))

instance (Show a) => Show (Grid a) where
  show = runIdentity . prettyPrintGrid (Identity . show)

fromListList :: [[a]] -> Grid a
fromListList = Grid . V.fromList . map V.fromList

toListList :: Grid a -> [[a]]
toListList (Grid vv) = map V.toList . V.toList $ vv

-- | Prints matrix.
prettyPrintGrid :: (Monad m) => (a -> m String) -> Grid a -> m String
prettyPrintGrid printCell (Grid g) = do
  g2 <- mapM (mapM printCell) . transpose . map V.toList $ V.toList g
  let g3 = map (Box.vcat Box.center1 . map Box.text) g2
      colCount = map Box.cols g3
      gStr = Box.render $ Box.hsep 1 Box.center1 $ intersperse3 verticleSep g3
      gStr2 = unlines . intersperse3 (horizontalSep colCount) . lines $ gStr
  pure gStr2
  where
    verticleSep :: Box.Box
    verticleSep = Box.vcat Box.center1 . replicate 9 $ Box.text "|"
    horizontalSep :: [Int] -> String
    horizontalSep (c1:c2:c3:c4:c5:c6:c7:c8:c9:_) =
      replicate (c1+c2+c3+3) '-'
        ++ '+' : replicate (c4+c5+c6+4) '-'
        ++ '+' : replicate (c7+c8+c9+3) '-'
    horizontalSep _ = undefined

    intersperse3 :: a -> [a] -> [a]
    intersperse3 a (x1:x2:x3:xs@(_:_)) = x1:x2:x3:a:(intersperse3 a xs)
    intersperse3 _ xs = xs

data Coordinate = Coordinate !Int !Int

instance Show Coordinate where
  show (Coordinate r c) = '(' : show r ++ ',' : show c ++ ")"

-- | Get the corresponding element of a grid.
gridElem :: Grid a -> Coordinate -> a
gridElem (Grid g) (Coordinate r c) = g ! r ! c
