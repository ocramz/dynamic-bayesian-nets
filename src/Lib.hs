{-# language OverloadedStrings #-}
module Lib
    -- (
    --   ourAdd
    -- )
    where

import Control.Applicative
-- import qualified Data.ByteString.Lazy as BL
-- import Data.Csv
-- import qualified Data.Vector as V

import Data.List
import Data.Maybe
import Data.Ord (comparing)

import qualified Data.Map as M


-- | a histogram function for lists
{-
hist ['a'] = [('a', 1)]
hist ['a', 'b'] = [('a', 0.5), ('b', 0.5)]
hist ['a', 'b', 'c'] = [('a', 1/3), ('b', 1/3), ('c', 1/3)]
hist ['a', 'b', 'c', 'a'] = [('a', 0.5), ('b', 0.25), ('c', 0.25)]
-}
hist :: (Ord a, Ord p, Fractional p) => [a] -> [(a, p)]
hist ll = sortBy (flip (comparing snd)) $ map f ll' where
  ll' = group (sort ll)
  n = length ll
  f e = (head e, fromIntegral (length e) / fromIntegral n)
  


-- | a map of item counts; it has a "size" nelbm for normalization. I.e. we can't just use `M.size` since Map removes duplicate elements
data BinMap a = BM { nelbm :: Int,
                     binmap :: M.Map a Int} deriving Show

emptyBM :: BinMap a
emptyBM = BM 0 M.empty

insertBM :: Ord a => a -> BinMap a -> BinMap a
insertBM x bm = BM (n + 1) mm' where
  mm = binmap bm
  n = nelbm bm
  nx = fromMaybe 0 (M.lookup x mm)  -- # of current elements in bin x
  mm' = M.insert x (nx + 1) mm      -- insert element and increase its count

insertFoldableBM :: (Foldable t, Ord a) => t a -> BinMap a -> BinMap a
insertFoldableBM xs bm = foldl' (flip insertBM) bm xs

fromFoldableBM :: (Foldable t, Ord a) => t a -> BinMap a
fromFoldableBM xs = insertFoldableBM xs emptyBM

-- a map for densities
data ProbMap a = ProbMap { probmap :: M.Map a Double} deriving Show

histBinMap :: BinMap a -> ProbMap a
histBinMap bm = ProbMap (M.map densf mm) where
  mm = binmap bm
  n = nelbm bm
  densf x = fromIntegral x / fromIntegral n

histPM :: (Foldable t, Ord k) => t k -> [(k, Double)]
histPM ll = M.toList pm where
 (ProbMap pm) = histBinMap (fromFoldableBM ll)



-- examples

-- bm0 = fromListBM "abca"




--

type Weight = Double

class (Functor d, Monad d) => Dist d where
  weighted :: [(a, Weight)] -> d a
