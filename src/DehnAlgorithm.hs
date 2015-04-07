
-- |
-- Module      : DehnAlgorithm
-- Description : An implementation of Dehn's algorithm
-- Copyright   : (c) Johannes Prem, 2014
-- License     : ISC License

module DehnAlgorithm
(
        dehnAlg
    ,   AllRelators
    ,   dehnAlg'
) where


import Control.Arrow ( (&&&), first )
import Data.List ( intercalate, stripPrefix )
import Data.List.Split ( splitOn )
import Data.Maybe ( listToMaybe, mapMaybe )
import Data.Monoid ( First(..), mconcat )

import PolygonScheme ( Scheme, invSch, reduce )
import Util ( rotations, slidingBlock )


-- | @'dehnAlg' rs w@ applies Dehn's algorithm
--   (<http://en.wikipedia.org/wiki/Small_cancellation_theory#Dehn.27s_algorithm Wikipedia>)
--   with relators @rs@ to the word @w@. Note, that @rs@ should /not/
--   include cyclic permutations because they are generated anyway.
--   (If you know what you're doing, you can use 'dehnAlg'' directly, see below.)
--   Examples:
--   
--   > mkSch xs = map toEnum xs :: Scheme
--   > r  = mkSch [1,1,2,2,3,3]  -- relator of the fundamental group of
--   >                           -- "the" non-orientable surface of genus 3
--   > w1 = mkSch [1,2,2,3,3,1]
--   > w2 = mkSch [1,2,2,3,3]
--   > w3 = mkSch [2,2,3,3,1,-3,-3,-2,-2,1,-3,-3,-2,-2,-1,-1,-1,-1]
--   >
--   > dehnAlg [r] w1 == []
--   > dehnAlg [r] w2 == mkSch [-1]
--   > dehnAlg [r] w3 == []
dehnAlg :: [Scheme] -> Scheme -> Scheme
dehnAlg []   sch = reduce sch
dehnAlg rels sch =
    dehnAlg' allRels minmaxRL' (reduce sch)
        where
            allRels   = prepareRels rels
            kk        = fst $ head allRels
            minmaxRL  = foldr (\ (k,_) (x,y) -> (min k x, max k y))
                              (kk,kk) allRels
            minmaxRL' = first (\ k -> k `div` 2 + 1) minmaxRL

-- | @[(length of relators in the second tuple component, list of relators)]@
type AllRelators = [(Int,[Scheme])]

-- takes a list of relators and adds all cyclic permutations
-- and their inverses; note, that we make no attempt to detect
-- or remove any duplicates here
prepareRels :: [Scheme] -> AllRelators
prepareRels = map (length &&& mkRels)
    where
        mkRels xs = rotations xs ++ rotations (invSch xs)

-- | This is the actual implementation of Dehn's algorithm.
dehnAlg' :: AllRelators -- ^ the relators to use; ensure that all cyclic
                        --   permutations and their inverses are included!
         -> (Int,Int)   -- ^ (min. relator length / 2 + 1, max. relator length)
         -> Scheme      -- ^ word to reduce via the algorithm
         -> Scheme      -- ^ output word; if this is not empty, the
                        --                input word is non-trivial
dehnAlg' _ _ [] = []
dehnAlg' rels rl@(minRLh,maxRL) sch =
    let nmax        = min maxRL (length sch)
        ns          = [nmax, nmax-1 .. minRLh]
        tryReplaceN = tryReplace rels sch
    in
        case mconcat $ map tryReplaceN ns of
            First Nothing     -> sch
            First (Just sch') -> dehnAlg' rels rl (reduce sch')

-- @'tryReplace' rels sch n@ tries to replace a subword of length @n@
-- in @sch@ with a shorter word (which is found by 'tryFind', see below).
-- It returns @First (Just sch')@ with the new word @sch'@,
-- or @First Nothing@ if there are no matches.
tryReplace :: AllRelators -> Scheme -> Int -> First Scheme
tryReplace rels sch n =
    First $ listToMaybe $ do
        subsch  <- slidingBlock n sch
        relrest <- mapMaybe (tryFind subsch n) rels
        return $ intercalate (invSch relrest) $ splitOn subsch sch

-- @'tryFind' subsch n (k, rs)@ tries to find the word @subsch@
-- of length @n@ as a prefix of one of the relators @rs@ of length k.
-- It returns the remaining suffix from the first matching relator,
-- or Nothing if there are no matches.
tryFind :: Scheme -> Int -> (Int, [Scheme]) -> Maybe Scheme
tryFind subsch n (k, rs)              -- subscheme is ...
    | n > k              =  Nothing   --   longer than relator
    | n < k `div` 2 + 1  =  Nothing   --   shorter than half of relator
    | otherwise          =
        listToMaybe $ mapMaybe (stripPrefix subsch) rs
