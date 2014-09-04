
module Util
(
      (.:)
    , both
    , equalLength
    , roundToMagnitude
    , filter2
) where

import Control.Arrow ( (***) )
import Control.Monad ( join )


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

filter2 :: (a -> Bool) -> (a -> Bool) -> [a] -> ([a],[a])
filter2 _ _ [] = ([],[])
filter2 p q (x:xs) =
    let (ys,zs) = filter2 p q xs
        ys' = if p x then x:ys else ys
        zs' = if q x then x:zs else zs
    in (ys',zs')
