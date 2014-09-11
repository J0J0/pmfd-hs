
-- |
-- Module      : Util
-- Description : Utility functions
-- Copyright   : (c) Johannes Prem, 2014
-- License     : ISC License

module Util
(
      (.:)
    , both
    , equalLength
    , roundToMagnitude
    , filter2
    , rotations
    , mapFirstN
    , slidingBlock
    , slidingBlock'
) where

import Control.Arrow ( (***) )
import Control.Monad ( join )
import Data.List ( inits, tails )


-- | An analog function to 'first' and 'second' from @Control.Arrow@:
both :: (a -> b) -> (a,a) -> (b,b)
both = join (***)

-- boobs operator  (is there an official name!?)
infixr 8 .:
(.:) = (.).(.)

-- | Test whether two lists have the same length.
equalLength :: [a] -> [b] -> Bool
equalLength [] [] = True
equalLength (_:xs) (_:ys) = equalLength xs ys
equalLength _ _ = False

roundToMagnitude :: (Num a, Ord a, Integral i) => a -> i
roundToMagnitude x =
    head $ dropWhile ((< x) . fromIntegral) $ iterate (*10) 1

-- | Like 'Prelude.filter' with two predicates.
--   
--   > filter p q xs == (filter p xs, filter q xs)
--   
--   but 'filter2' traverses the list only once.
filter2 :: (a -> Bool) -> (a -> Bool) -> [a] -> ([a],[a])
filter2 _ _ [] = ([],[])
filter2 p q (x:xs) =
    let (ys,zs) = filter2 p q xs
        ys' = if p x then x:ys else ys
        zs' = if q x then x:zs else zs
    in (ys',zs')

-- | Returns all rotations of a list, e. g.:
-- 
--   > rotations [1..4] == [[1,2,3,4],[2,3,4,1],[3,4,1,2],[4,1,2,3]]
rotations :: [a] -> [[a]]
rotations xs =
    init $ zipWith (++) (tails xs) (inits xs)
-- from http://stackoverflow.com/a/7632272

-- | Map a function over the first @n@ elements of a list.
mapFirstN :: Int -> (a -> a) -> [a] -> [a]
mapFirstN _ _ []     = []
mapFirstN 0 _ xs     = xs
mapFirstN k f (x:xs) = f x : mapFirstN (k-1) f xs

-- | @'slidingBlock' k xs@ returns all contiguous sublists of xs that
--   consist of exactly k elements, e. g.:
--   
--   > slidingBlock 3 [1..5] == [[1,2,3],[2,3,4],[3,4,5]]
--   
--   If you already know the length of the list, use 'slidingBlock'' directly.
slidingBlock :: Int -> [a] -> [[a]]
slidingBlock slen xs = slidingBlock' (length xs) slen xs

-- | Like 'slidingBlock' but expects the length of the list as first argument.
slidingBlock' :: Int -> Int -> [a] -> [[a]]
slidingBlock' len slen
    | slen < 0    =  error "slidingBlock: negative block length"
    | slen > len  =  error "slidingBlock: block longer than list"
    | otherwise   =  partA (slen) (len-slen) 1

-- @'slidingBlock'' len slen xs@ works as follows:
-- Let's say @xs@ is given by
--      1,2,...,n;
-- first the list is (notationally) split into three parts, depending
-- on the length @slen@ of the sliding block. We let @ka = len-slen@
-- and @kb = slen@ and split the list like this:
-- Case: slen >= len `div` 2
--       part A  |   part B    |   part C
--      1,...,ka | ka+1,...,kb | kb+1,...,n
--   Then the functions partA, partB and partC treat each part accordingly
--   to obtain our desired blocks:
--      1,...,kb,
--      2,...,kb+1
--      and so on, up to
--      ka+1,...,n.
-- Case: slen < len `div` 2
--       part A  |   part B    |   part C
--      1,...,kb | kb+1,...,ka | ka+1,...,n
--   Then partA, partB' and partC treat each part accordingly to obtain the
--   same blocks as in the first case.
--
-- In the code below, ka and kb are actually used as counters:
-- ">=" case: partA is finished as soon as ka == 0 and
--            partB needs to take kb steps;
-- "<"  case: partA is finished as soon as kb == 0 and
--            partB' needs to take ka steps.


-- treats part A;
-- note that kb+j == slen+1 at any time
-- (when called correctly from slidingBlock')
partA :: Int -> Int -> Int -> [a] -> [[a]]
partA kb  0 _ xs     = partB kb xs
partA  0 ka j xs     = partB' (j-1) ka xs
partA kb ka j (x:xs) =
    mapFirstN l (x:) $ partA (kb-1) (ka-1) (j+1) xs
        where
            l = min (kb+j-1) j

-- treats part B in the ">=" case
partB :: Int -> [a] -> [[a]]
partB  0 xs     = partC xs
partB kb (x:xs) =
    map (x:) (partB (kb-1) xs)

-- treats part B in the "<" case
partB' :: Int -> Int -> [a] -> [[a]]
partB' l  0 xs     = partC xs
partB' l kb (x:xs) =
    [] : mapFirstN l (x:) (partB' l (kb-1) xs)

-- treats part C
partC :: [a] -> [[a]]
partC = inits
