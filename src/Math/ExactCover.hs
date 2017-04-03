-- |
--
-- This module provides an efficient solver for exact set cover problems
-- (<http://en.wikipedia.org/wiki/Exact_cover>) using Algorithm X as described
-- in the paper /Dancing Links/, by Donald Knuth, in
-- /Millennial Perspectives in Computer Science/, P159, 2000
-- (<https://arxiv.org/abs/cs/0011047>).
--
-- For a quick start, go straight to the 'solve' function.

module Math.ExactCover
  (
    -- * Mathematical definition
    -- $def

    -- * Simple interface
    solve

    -- * Types
  , ExactCoverProblem

    -- * Construction
  , transform

    -- * Solvers
  , solveEC
  )
where

import Math.ExactCover.Internal.DLX

import Foreign
import System.IO.Unsafe (unsafePerformIO)
import Control.Concurrent (MVar, newMVar, withMVar)
import Control.Monad (forM, forM_, when)
import Data.Map (Map)
import qualified Data.Map.Strict as Map (lookup, keys)
import Data.Set (Set)
import qualified Data.Set as Set (size, toList, fromList)


-- | Basic type that represents the exact cover problem.
data ExactCoverProblem setlabel a = ExactCoverProblem
  { ec_sets :: !(Map (Set a) setlabel)  -- ^ Associates each set with a label.
  , ec_dlx :: MVar (ForeignPtr DLXMatrix)  -- ^ Dancing links representation.
  }

-- PROGRAMMING NOTE: The Enum typeclass is needed as the set objects are
-- internally represented as integers in the C portion of the library.

-- | Constructs an 'ExactCoverProblem' given a collection of subsets \\(
-- \\mathcal{S} \\) as represented by a 'Map' between each subset and its label.
-- The set \\( \\mathcal{X} \\) over which the exact cover is to be found is
-- assumed to be the union of the given collection of subsets \\( \\mathcal{S}
-- \\).
transform :: (Enum a)
          => Map (Set a) setlabel
          -> ExactCoverProblem setlabel a
transform m = unsafePerformIO $ do
  dlxHead <- newForeignPtr c_free_matrix =<< c_create_empty_matrix
  withForeignPtr dlxHead $ \hPtr ->
    forM_ (Map.keys m) $ \k ->
      withArray (fromIntegral . fromEnum <$> Set.toList k) $ \constrainPtr ->
        with hPtr $ \hPPtr -> do
          ret <- c_add_set hPPtr constrainPtr (fromIntegral $ Set.size k)
          when (ret /= 0) $ error "Could not add constraint."
  dlxM <- newMVar dlxHead
  pure $ ExactCoverProblem { ec_sets = m
                           , ec_dlx = dlxM
                           }

-- | Solves the given 'ExactCoverProblem', returning the labels of the subsets
-- that form the exact cover.
solveEC :: (Enum a, Ord a)
        => ExactCoverProblem setlabel a
        -> [setlabel]
solveEC ExactCoverProblem{ ec_sets = setLabels, ec_dlx = dlxM } = unsafePerformIO $
  withMVar dlxM $ \dlxHead ->
  withForeignPtr dlxHead $ \hPtr ->
  alloca $ \setCoversPtrPtrPtr ->
  alloca $ \setCoverSizesPtrPtr ->
  alloca $ \nSetsPtr -> do
    ret <- c_solve hPtr 4 setCoversPtrPtrPtr setCoverSizesPtrPtr nSetsPtr
    case () of
      _ | ret < 0 -> error ""
        | ret == 0 -> do
            -- Retrieve number of result sets.
            nSets <- fromIntegral <$> peek nSetsPtr

            -- Retrieve list of set sizes.
            setCoverSizesPtr <- peek setCoverSizesPtrPtr
            setCoverSizes <- pure . map fromIntegral
                             =<< peekArray nSets setCoverSizesPtr
            free setCoverSizesPtr

            -- Retrieve list of pointers to the result sets.
            setCoversPtrPtr <- peek setCoversPtrPtrPtr
            setCoversPtr <- peekArray nSets setCoversPtrPtr
            free setCoversPtrPtr

            -- Retrieve the result sets.
            forM (zip setCoverSizes setCoversPtr) $ \(setSize, setPtr) -> do
              setC <- Set.fromList . map (toEnum . fromIntegral)
                      <$> peekArray setSize setPtr
              free setPtr
              pure $ case Map.lookup setC setLabels of
                Nothing -> error "Constrain set not in map."
                Just label -> label

        | ret == 1 -> pure mempty
        | otherwise -> error "Unknown error code."

-- | Given a collection of subsets \\( \\mathcal{S} \\), represented by a 'Map'
-- between each subset (of type @'Set' a@) and its label, returns a list of
-- labels that represents the exact cover \\( \\mathcal{S}^{*} \\).
--
-- Example: To find the exact cover of the collection of subsets
-- \\( \\left\\{\\left\\{2,4,5\\right\\}, \\left\\{0,3,6\\right\\},
-- \\left\\{1,2,5\\right\\}, \\left\\{0,3\\right\\}, \\left\\{1,6\\right\\},
-- \\left\\{3,4,6\\right\\}\\right\\} \\),
--
-- > solve (Map.fromList [ (Set.fromList [2,4,5], 'A')
-- >                     , (Set.fromList [0,3,6], 'B')
-- >                     , (Set.fromList [1,2,5], 'C')
-- >                     , (Set.fromList [0,3], 'D')
-- >                     , (Set.fromList [1,6], 'E')
-- >                     , (Set.fromList [3,4,6], 'F')
-- >                     ] :: Map (Set Int) Char)
-- > == "DAE"
solve :: (Enum a, Ord a) => Map (Set a) setlabel -> [setlabel]
solve = solveEC . transform

-- $def
--
-- Given a collection \\( \\mathcal{S} \\) of subsets of a set \\( \\mathcal{X}
-- \\), an exact cover is a subcollection \\( \\mathcal{S}^{*} \\) of \\(
-- \\mathcal{S} \\) such that each element in \\( \\mathcal{X} \\) is contained
-- in exactly one subset in \\( \\mathcal{S}^{*} \\) (from wikipedia).
