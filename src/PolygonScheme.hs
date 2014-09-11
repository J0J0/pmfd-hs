
-- |
-- Module      : PolygonScheme
-- Description : Labelling schemes polygons
-- Copyright   : (c) Johannes Prem, 2014
-- License     : ISC License
-- 
-- Every closed surface can be obtained from a polygon
-- with an even number of edges where the latter are
-- identified in pairs. Such an identification can be
-- described by a so called /labelling scheme/. For
-- instance, the torus can be obtained from a square
-- by identifying the edges according to the following
-- labelling scheme: @aba^-1b^-1@. Because @^-1@ is
-- ugly, we write @a'@ for the inverse of the symbol
-- @a@ in the following.
-- 
-- >                        b
-- >   --------          -------- 
-- >   |      |   ~>   a |      | a'   ~>   torus
-- >   |      |          |      |
-- >   --------          --------
-- >                        b'
-- 
-- This module provides functionality for dealing with
-- such schemes, namely for normalizing and
-- canonicalizing them. If we are given a loop on the
-- polygon that traverses some of the edges in a
-- particular order, we are also interested in
-- representing this loop with the symbols of our
-- normalized scheme. Thus we include support for
-- (what we call) /tracing a loop through the
-- normalization process/.
-- 
-- Our main reference for the topic is Chapter 12 in
-- the book \"Topology\" by Munkres. The algorithm for
-- normalizing schemes is stated in the exercises after
-- § 77 and the information how to modify the loops
-- while normalizing was extracted directly from Lemmas
-- 77.1, 77.3 and 77.4 of § 77.

module PolygonScheme
(
      Symbol
    , Scheme
    , SchemeWL(..)
    , toSchemeWL
    , invSymb
    , invSch
    , isProperScheme
    , reduce
    , normalizeScheme
    , normalizeSchemeWL
    , renameScheme
    , canonicalizeScheme
) where

import Control.Arrow ( first, second )
import Control.Monad.State
import Data.Char ( chr )
import qualified Data.Foldable as F ( all )
import Data.List ( foldr, reverse )
import Data.Maybe ( fromMaybe )
import qualified Data.Map.Strict as M

newtype Symbol = Symbol Integer deriving (Eq, Ord)
type Scheme = [Symbol]

-- | For the symbols with underlying integers
--   from 1 to 26 we show the letters @a@ to @z@
--   and @a'@ to @z'@ for their inverses. Other
--   symbols are shown by their numeric value.
instance Show Symbol where
    show (Symbol k)
        | absk <= 26  = char : maybeInv
        | otherwise   = show k
            where
                absk = abs $ fromIntegral k
                char = chr $ absk + 96   -- 97 is ascii 'a'
                maybeInv = if k < 0
                           then "'"
                           else []

instance Enum Symbol where
    toEnum k = Symbol $ fromIntegral k
    fromEnum (Symbol k) = fromIntegral k


-- | Formal inverse of a symbol.
invSymb :: Symbol -> Symbol
invSymb (Symbol k) = Symbol (-k)

-- | Formal inverse of a scheme.
invSch :: Scheme -> Scheme
invSch = reverse . map invSymb


-- | Returns 'True' if the scheme is a proper scheme.
--   A scheme is proper if all symbols occure exactly
--   twice (possibly with different sign).
isProperScheme :: Scheme -> Bool
isProperScheme =
    F.all (==2) .
        foldr (\ (Symbol k) -> M.insertWith (+) (abs k) 1) M.empty


-- Split a scheme after leading pairs of the same symbol, e. g.
-- @aabbcdef@ is split into @aabb@ and @cdef@.
splitAfterLastPair :: Scheme -> (Scheme, Scheme)
splitAfterLastPair xs@(x:xx:xs')
    | x == xx    =
        let (ps,rs) = splitAfterLastPair xs'
        in (x:x:ps,rs)
splitAfterLastPair xs = ([],xs)

-- Like 'splitAfterLastPair' but split right /before/ the last pair,
-- e. g. @aabbcdef@ is split into @aa@ and @bbcdef@.
splitBeforeLastPair :: Scheme -> (Scheme, Scheme)
splitBeforeLastPair (x:xx:xs'@(y:yy:_))
    | x == xx && y == yy  =
        let (ps, rs) = splitBeforeLastPair xs'
        in (x:xx:ps, rs)         
splitBeforeLastPair xs = ([], xs)


-- normalizing a scheme

-- | Freely 'reduce's a scheme, i. e. all occurences of symbols
--   followed by their inverse are deleted. This is done until
--   no further reductions are possible, e. g.
--   @abb'a'@ is reduced to the empty scheme.
reduce :: Scheme -> Scheme
reduce sch = evalState (step2h False [] sch) []  -- see below for step2h

-- | A labelling scheme together with a loop specified
--   in the symbols of the scheme (or 'Nothing').
data SchemeWL  = SchemeWL {   scheme :: Scheme
                            , sloop  :: Maybe Scheme }
                 deriving (Show)

loop :: SchemeWL -> Scheme
loop = fromMaybe [] . sloop

toSchemeWL :: Scheme -> SchemeWL
toSchemeWL sch = SchemeWL sch Nothing


-- The functions below primarly compute the normal form of a scheme
-- but their secondary function is to trace a loop through this process.
-- Therefore they pass the loop in question around in the state.
type SchemeWLst = State Scheme Scheme

-- modify the state loop

-- Replace all occurences of the symbol @s@ by the scheme @sch@
-- where signs are considered, i. e. 'invSymb s' is also replaced
-- by 'invSch sch'.
subs1 :: Symbol -> Scheme -> Scheme -> Scheme
subs1 s sch =
    subs' (\ x -> case () of
                    _ | x == s    ->  sch
                      | x == is   -> isch
                      | otherwise ->  [x] )
        where
            is = invSymb s
            isch = invSch sch

-- Analogous to subs1, but replace two symbols.
subs2 :: Symbol -> Scheme -> Symbol -> Scheme -> Scheme -> Scheme
subs2 s1 sch1 s2 sch2 =
    subs' (\ x -> case () of
                    _ | x == s1   ->  sch1
                      | x == is1  -> isch1
                      | x == s2   ->  sch2
                      | x == is2  -> isch2
                      | otherwise ->   [x] )
        where
            is1 = invSymb s1
            is2 = invSymb s2
            isch1 = invSch sch1
            isch2 = invSch sch2

-- helper for subs1 and subs2
subs' :: (Symbol -> Scheme) -> Scheme -> Scheme
subs' f sch = sch >>= f

-- statefull version of 'subs1' that also reduces afterterwards
subs1rS :: Symbol -> Scheme -> State Scheme ()
subs1rS s sch = modify (reduce . subs1 s sch)

-- analogous to 'subs1rS'
subs2rS :: Symbol -> Scheme -> Symbol -> Scheme -> State Scheme ()
subs2rS s1 sch1 s2 sch2 = modify (reduce . subs2 s1 sch1 s2 sch2)

-- Here comes the code that does the actual scheme normalization.
-- 
-- All steps assume that the previous steps have been executed in
-- the correct order, so you should never do @step3 sch@ but only
-- @step3 . step2 . step1 $ sch@.

-------------------------------------------------
-- move aa in front
--      y0 a y1 a y2
--  ~>  a a y0 y1' y2
-- 
step1 :: Scheme -> SchemeWLst
step1 = step1h [] [] 

step1h :: Scheme -> Scheme -> Scheme -> SchemeWLst
step1h pairs pre [] = return $ pairs ++ reverse pre
step1h pairs pre (x:xs) =
    case break (==x) xs of
        (_,[])      -> step1h pairs (x:pre) xs
        (ys,(_:zs)) -> do
                        unless (null ys && (null pre || null zs)) $
                               subs1rS x (invSch $ zs ++ [x] ++ pre)
                        step1h (x:x:pairs) [] (reverse pre ++ invSch ys ++ zs)


-------------------------------------------------
-- remove aa'
-- see the description of 'reduce' above;
-- the difference is that schemes of length
-- less than 5 won't be reduced any further.
-- 
step2 :: Scheme -> SchemeWLst
step2 = step2h True []

step2h :: Bool -> Scheme -> Scheme -> SchemeWLst
step2h check s1 s2
    | len2 < 2 || (check && len1 + len2 <= 4) =
        return $ reverse s1 ++ s2
        where
            len1 = length s1
            len2 = length s2
step2h check s1 (y:yy:ys)
    | y == invSymb yy   = do subs1rS y []
                             case s1 of
                                []      -> step2h check [] ys
                                (x:xs)  -> step2h check xs (x:ys)
    | otherwise         = step2h check (y:s1) (yy:ys)
    
    
-------------------------------------------------
-- group aba'b' together
--      w0 y1 a y2 b y3 a' y4 b' y5 
--  ~>  w0 abab' y1 y4 y3 y2 y5
--
step3 :: Scheme -> SchemeWLst
step3 s = do
        rest' <- step3h [] rest
        return $ pairs ++ rest'
            where
                (pairs,rest) = splitAfterLastPair s

step3h :: Scheme -> Scheme -> SchemeWLst
step3h y1r xs
    | length xs <= 4  = return $ reverse y1r ++ xs
step3h y1r (x:xs) = do
    let (ys,_:zs) = break (== (invSymb x)) xs
    case step3hh [] ys zs of
        Nothing              -> step3h (x:y1r) xs
        Just (y,y2,y3,y4,y5) -> do
            let ix = invSymb x
                iy = invSymb y
                y1 = reverse y1r
                rest = y1 ++ y4 ++ y3 ++ y2 ++ y5
            subs1rS x (invSch $ y2 ++ [y] ++ y3 ++ [ix] ++ y1)
            subs2rS y (invSch $ y3 ++ [y,ix,iy] ++ y2)
                    x [iy]
            subs2rS y (invSch $ [y,ix,iy] ++ y1 ++ y4 ++ y3)
                    x [iy]
            rest' <- (return rest >>= step2h False [] >>= step3h [])
            return $ [x,y,ix,iy] ++ rest'
    where
        step3hh :: Scheme -> Scheme -> Scheme -> 
                        Maybe (Symbol,Scheme,Scheme,Scheme,Scheme)
        step3hh _ [] _ = Nothing
        step3hh pre (y':ys') zs =
            case break (== (invSymb y')) zs of
                (_,[])        -> step3hh (y':pre) ys' zs
                (zs',_:zs'')  -> Just (y', reverse pre, ys', zs', zs'')


-------------------------------------------------
-- convert ccaba'b' to aabbcc
--       w0 cc aba'b' w1
--   ~>  w0 aabbcc
--
step4 :: Scheme -> SchemeWLst
step4 xs
    | length xs <= 4  = return xs
step4 xs@(x:xx:_)
    | x /= xx    = return xs
    | otherwise  = do let (pairs, (x:_:rest)) = splitBeforeLastPair xs
                      step4h pairs x rest

step4h :: Scheme -> Symbol -> Scheme -> SchemeWLst
step4h w0 x [] = return $ w0 ++ [x,x]
step4h w0 x (y:z:_:_:rest') = do
    let w1w0 = rest' ++ w0
    subs1rS x (invSch $ [y,z,x] ++ w1w0)
    subs1rS z (invSch $ [y,x] ++ w1w0 ++ [z,y])
    subs1rS y (invSch $ [x] ++ w1w0 ++ [y,z,z])
    step4h (w0 ++ [y,y,z,z]) x rest'


-------------------------------------------------
-- handle schemes with four symbols
-- (loop tracing isn't important here, since the
-- word problem for surfaces with four symbols is
-- a special case anyway)
-- 
step5' :: Scheme -> Scheme
step5' xs@[x,xx,y,yy]
    | x == xx         &&  y == yy          =  [x,invSymb y,x,y]  -- Klein bottle
    | x == xx         &&  y == invSymb yy  =  [x,y,x,y]          -- projective plane
    | x == invSymb yy && xx == invSymb y   =  [yy,x,xx,y]        -- sphere
step5' xs = xs

step5 :: Scheme -> SchemeWLst
step5 = return . step5'


-------------------------------------------------
-- now compose all steps and wrap it up in
-- suitable way

-- | Take a scheme with loop, normalizes the
--   scheme and ajust the loop scheme acordingly.
normalizeSchemeWL :: SchemeWL -> SchemeWL
normalizeSchemeWL sch =
    SchemeWL sch' (sloop sch >> Just loop')
    where
        (sch', loop') = runState (allSteps $ scheme sch) (loop sch)
        allSteps sch  = return sch >>= step1 >>= step2
                       >>= step3 >>= step4 >>= step5

-- | A function for convenience to only normalize a scheme.
normalizeScheme :: Scheme -> Scheme
normalizeScheme =
    scheme . normalizeSchemeWL . toSchemeWL


-------------------------------------------------
-- rename symbols to obtain canonical form

-- | A normalized scheme may for example still start with
--   an "inverse symbol" (i. e. a symbol with a negative
--   underlying integer). This function renames the symbols
--   in a scheme to achieve a true canonical form, e. g.
--   @z'z'ccr'r'@ represents a non-orientable surface of
--   genus 3 but the more canonical scheme representation
--   would be @aabbcc@. The second ouput component is a
--   translating function (which might be useful), e. g.
--   in the example above @z@ is mapped to @a'@,
--   @c@ to @b@ and @r@ to @c'@ (all other symbols are
--   passed though untouched).
--   
--   The only exception is the Klein bottle where we
--   consider @ab'ab@ to be the canonical representation.
renameScheme :: Scheme -> (Scheme, Symbol -> Symbol)
-- handle the only exception (Klein bottle)
renameScheme xs@[x,xx,y,yy]
    | x == y && xx == invSymb yy  =  (map rf xs, rf)
        where
            ix = invSymb x
            rf z | z == x     =  toEnum 1
                 | z == xx    =  toEnum (-2)
                 | z == ix    =  toEnum (-1)
                 | z == yy    =  toEnum 2
                 | otherwise  =  z
-- otherwise run the renaming algorithm
renameScheme xs =
    let (xs', (m, _)) = runState (renameSchemeH xs) (M.empty, toEnum 1)
    in (xs', renameFunctionFromMap m)


-- RenameState stores the current re-mapping of symbols
-- and the next unused symbol.
-- The re-mapping is stored as follows in the @Map Integer Symbol@:
-- a key is the absolute value @k@ of a symbol's integer and
-- the corresponding value is the symbol @x@ such that
-- @Symbol k@ is mapped to @x@. Then it is implicit that
-- @Symbol (-k)@ must be mapped to @invSymb x@.
type RenameState = (M.Map Integer Symbol, Symbol)

rnLookup :: Integer -> State RenameState (Maybe Symbol)
rnLookup k = gets (M.lookup k . fst)
rnBecomes :: Integer -> Symbol -> State RenameState ()
k `rnBecomes` x = modify $ first $ M.insert k x
rnNextSymb :: State RenameState Symbol
rnNextSymb = do
    (m, x) <- get
    put (m, succ x)
    return x

-- This function applies the correct sign to a symbol
-- that we looked up in the re-mapping state. See also
-- the description of 'RenameState' above.
rnSgn :: Integer -> Symbol -> Symbol
rnSgn k x = if k < 0 then invSymb x else x

-- This function handles the actual renaming.
renameSchemeH :: Scheme -> State RenameState Scheme
renameSchemeH [] = return []
renameSchemeH (x@(Symbol k):xs) = do
    let ak = abs k
    jy <- rnLookup ak
    case jy of
        (Just y) -> do rest <- renameSchemeH xs
                       return (rnSgn k y:rest)
        Nothing  -> do y <- rnNextSymb
                       ak `rnBecomes` (rnSgn k y)
                       rest <- renameSchemeH xs
                       return (y:rest)

-- produce a renaming function as described above from
-- our internally stored @Map Integer Symbol@.
renameFunctionFromMap :: (M.Map Integer Symbol) -> Symbol -> Symbol
renameFunctionFromMap m x@(Symbol k) =
    case M.lookup (abs k) m of
        Nothing  -> x
        (Just y) -> rnSgn k y

-- | This function combines normalization and renaming.
canonicalizeScheme :: Scheme -> Scheme
canonicalizeScheme = fst . renameScheme . normalizeScheme
