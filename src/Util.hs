
module Util
(
      (.:)
    , both
    , equalLength
    , roundToMagnitude
) where

import Control.Arrow ( (***) )
import Control.Monad ( join )


-- analogous to 'first' and 'second' from Control.Arrow:
both :: (a -> b) -> (a,a) -> (b,b)
both = join (***)

infixr 8 .:
(.:) = (.).(.)

equalLength :: [a] -> [b] -> Bool
equalLength [] [] = True
equalLength (_:xs) (_:ys) = equalLength xs ys
equalLength _ _ = False

roundToMagnitude :: (Num a, Ord a, Integral i) => a -> i
roundToMagnitude x =
    head $ dropWhile ((< x) . fromIntegral) $ iterate (*10) 1
