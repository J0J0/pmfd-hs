{-# LANGUAGE LambdaCase, StandaloneDeriving #-}

-- |
-- Module      : TwoDimManifold
-- Description : Deal with complexes that triangulate closed surfaces
-- Copyright   : (c) Johannes Prem, 2014
-- License     : ISC License
-- 
-- Note that most of the functions in this module require their input complex
-- to be a 2-dimensional manifold, i. e.
-- 'TwoDimPseudoManifold.isTwoDimManifold' would return 'True'. Do not call
-- those functions with other complexes (or if you do, start saving money for a
-- new computer :D).

module TwoDimManifold
(
      PreScheme
    , ComplexWL(..)
    , toComplWL
    , polygonScheme
    , polygonSchemeWL
    , polygonPSchemeWL
    , polygonSchemeAtSimplex
    , identifySurface
    , buildSchemeWL
    , preSchemeToScheme
    , preSchemeToScheme'
    , translateVia
) where

import Control.Arrow ( second )
import Control.Monad.State
import Data.List (   
                     (\\)
                   , delete
                   , foldr
                   , mapAccumR
                   , nubBy
 )
import Data.Maybe ( fromMaybe, fromJust, isNothing )

import SimplicialComplex (
                             Vertex(..)
                           , Simplex
                           , Complex
                           , dfsSimplices
                           , dimS
                           , isFaceOf
                           , isNSimplex
                           , parentSimplices
 )

import PolygonScheme ( 
                         Symbol
                       , Scheme
                       , SchemeWL(..)
                       , invSymb
                       , normalizeSchemeWL
 )

import Surface (
                   Surface
                 , identifySurfaceScheme
 )
 
import Util ( (.:) )


-- | A 'PreScheme' is used to represent a walk in a complex.
--   Much lika a 'Scheme' (from "PolygonScheme") represents
--   the edges of a polygon by formal symbols, a walk in a
--   complex can be specified by consecutive edges of a complex,
--   that is, by oriented 1-simplices that fit together.
--   Because of these similarities, this type is called /prescheme/.
--   
--   For example, @[ [1,2], [2,3], [3,1] ] :: PreScheme@
--   specifies a closed walk from the vertex @1@ to itself along three
--   edges of a suitable complex.
--   
--   Note that in this case the odering of a 1-simplices is important
--   and that @[3,1]@ is /not/ the same edge as @[1,3]@.
type PreScheme a = [Simplex a]

-- | A complex together with a \"loop\" in it,
--   specified by a prescheme (or 'Nothing').
--   The intuition should always be that @cloop@ is indeed a cycle
--   in the complex but for computational reasons it might be
--   preferable to store only a walk in @cloop@, which is also
--   fine for the functions that use this type.
data ComplexWL a = ComplexWL {   complex :: Complex a
                               , cloop   :: Maybe (PreScheme a) }

deriving instance (Show a) => Show (ComplexWL a)

loop :: ComplexWL a -> PreScheme a
loop = fromMaybe [] . cloop

toComplWL :: Complex a -> ComplexWL a
toComplWL c = ComplexWL c Nothing


-- utility function that returns a loop (as a 'PreScheme') around
-- the given 2-simplex.
twoSimplexBoundary :: Simplex a -> PreScheme a
twoSimplexBoundary s =
    [[s!!0,s!!1], [s!!1,s!!2], [s!!2,s!!0]]


-- | 'identifySurface' takes a complex that triangulates a closed
--   surface and determines the latter. To that end, it computes
--   the normalized polygon scheme for the input complex (using
--   'polygonScheme') and passes the result to 'identifySurfaceScheme'
--   from "Surface".
--   
--   The input complex must be a 2-dim. manifold.
identifySurface :: Complex a -> Surface
identifySurface = identifySurfaceScheme . polygonScheme


-- FIXME: Since the support for tracing loops through the
--        normalization process was added afterwards, the whole
--        polygonScheme stuff needs a refactoring.
--        Also the conversion from PreScheme to Scheme should happen
--        much later and, ideally, PreScheme/Scheme would be
--        clearly separated while both should be exported since we
--        need direct access to PreSchemes in TwoDimPseudoManifold.Loop.
--        'polygonPSchemeWL' is just a quick hack used in the latter
--        module ... :(
 
-- | Computes the normalized polygon scheme for the input manifold.
-- 
--   The input complex must be a 2-dim. manifold.
polygonScheme :: Complex a -> Scheme
polygonScheme =
    scheme . normalizeSchemeWL . polygonSchemeWL . toComplWL

-- | Computes the normalized polygon scheme for the input manifold
--   and a representation of the attached walk in terms of the
--   scheme symbols.
-- 
--   The input complex must be a 2-dim. manifold.
polygonSchemeWL :: ComplexWL a -> SchemeWL
polygonSchemeWL c =
    polygonSchemeAtSimplex c $ head $ delete [] $ complex c

-- meh, duplicate code, see FIXME above ...
-- | (Temporary utility function -- don't actually use this!)
polygonPSchemeWL :: Complex a -> PreScheme a -> (PreScheme a, PreScheme a)
polygonPSchemeWL c loop =
    runState (buildPSchemeWL initPSch ss []) loop
        where
            s'     = head $ filter (isNSimplex 2) c
            (s:ss) = reverse $ dfsSimplices c s'
            initPSch = twoSimplexBoundary s


-- Compute the polygon scheme of a manifold starting at any 2-simplex
-- containting the input simplex.
-- 
-- The input complex must be a 2-dim. manifold.
polygonSchemeAtSimplex :: ComplexWL a -> Simplex a -> SchemeWL
polygonSchemeAtSimplex c s 
    | dimS s == 2   = polygonSchemeAtTwoSimplex c s
    | otherwise     = polygonSchemeAtTwoSimplex c s'
        where
            s' = head $ filter (isNSimplex 2) $ parentSimplices s (complex c)

-- Compute the polygon scheme of a manifold starting at a specific
-- 2-simplex.
--
-- The input complex must be a 2-dim. manifold.
polygonSchemeAtTwoSimplex :: ComplexWL a -> Simplex a -> SchemeWL
polygonSchemeAtTwoSimplex c s =
    buildSchemeWL (cloop c) . reverse $ dfsSimplices (complex c) s

-- Takes all 2-simplices of a (connected component of) a manifold and
-- builds a polygon scheme by pasting the 2-simplices together.
buildScheme :: [Simplex a] -> Scheme
buildScheme = scheme . buildSchemeWL Nothing

-- Like 'buildScheme' but also supports walk handling, see
-- the description of 'polygonSchemeWL'.
buildSchemeWL :: Maybe (PreScheme a) -> [Simplex a] -> SchemeWL
buildSchemeWL mbloop ss =
    fst $ buildSchemeWL' mbloop ss

-- Like 'buildSchemeWL' but also contains the translation \"table\"
-- used to transform the 'PreScheme' into a 'Scheme'.
buildSchemeWL' :: Maybe (PreScheme a) -> [Simplex a] -> (SchemeWL, [(Simplex a, Symbol)])
buildSchemeWL' mbloop (s:ss) =
    (SchemeWL sch loop'', tab)
        where
            initPSch = twoSimplexBoundary s
            loop = fromMaybe [] mbloop
            (pSch, loop') = runState (buildPSchemeWL initPSch ss []) loop
            (sch, tab) = preSchemeToScheme pSch
            loop'' = mbloop >> Just (translateVia tab loop')

-- This is the function that actually does the pasting of 2-simplices
-- until no more are available. Note that the input 2-simplices
-- /must/ \"fit together\" for this function to terminate. Otherwise
-- it will just run forever, trying to paste some 2-simplex to the
-- current polygon (scheme).
buildPSchemeWL :: PreScheme a -> [Simplex a] -> [Simplex a] ->
                    State (PreScheme a) (PreScheme a)
buildPSchemeWL curPSch [] [] = return curPSch
buildPSchemeWL curPSch [] ss' = buildPSchemeWL curPSch ss' []
buildPSchemeWL curPSch (s:ss) ss' = do
    tryPasteSimplex s s curPSch >>= \case
        Nothing   -> buildPSchemeWL curPSch ss (s:ss')
        Just pSch -> buildPSchemeWL pSch ss ss'

-- FIXME: tryPasteSimplex is used for both pastings (polygon, loop)
--        (because the code would be almost the same)
--        but the case switch with "when" is rather hacky;
--        maybe find a better solution!?
--        (... or even better, do the complete refactoring mentioned
--        in the last fixme!)

-- (The following description applies to the pasting of 2-simplices
--  into the polygon (scheme). See 'adjustLoop' below for an
--  explanation about loop adjustment via this function.)
--  
--  Takes a 2-simplex (and the same simplex again) and the
--  prescheme that describes the current polygon. Then it tries
--  to paste the 2-simplex to the polygon and returns @'Just'
--  newPreScheme@ in case of success and 'Nothing' otherwise.
tryPasteSimplex :: Simplex a -> Simplex a -> PreScheme a -> 
                    State (PreScheme a) (Maybe (PreScheme a))
tryPasteSimplex s s' sch =
    case break (`isFaceOf` s) sch of
      (_,[])            -> return Nothing
      (sch', (e:sch'')) -> do let [v1,v2] = e
                                  [v3] = s' \\ e
                              when (dimS s == 2) $ adjustLoop e s
                              return $ Just (sch' ++ [[v1,v3], [v3,v2]] ++ sch'')

-- 'adjustLoop' is called from 'tryPasteSimplex' to update
-- the loop that we have to trace through the normalization.
-- However, it also calls 'tryPasteSimplex' because the code is
-- virtually the same for both pastings. To ensure that this doesn't
-- result in an infinite loop, 'tryPasteSimplex' skips the call to
-- 'adjustLoop' when operating in \"loop pasting mode\".
adjustLoop :: Simplex a -> Simplex a -> State (PreScheme a) ()
adjustLoop e s = do
    get >>= tryPasteSimplex e s >>= \case
        Nothing -> return ()
        Just l' -> do put l'
                      adjustLoop e s


-- Convert a PreScheme to a Scheme and additionally return the
-- translation table.
preSchemeToScheme :: PreScheme a -> (Scheme, [(Simplex a, Symbol)])
preSchemeToScheme = preSchemeToScheme' 1
    
-- Like 'preSchemeToScheme' but start with a specific symbol.
preSchemeToScheme' :: Int -> PreScheme a -> (Scheme, [(Simplex a, Symbol)])
preSchemeToScheme' t pSch =
    (translateVia table pSch, table)
        where
            table = nubBy isFaceOf pSch `zip` ([toEnum t ..] :: [Symbol])

-- Use the translation table given as first argument to transform
-- the PreScheme of the second argument to a Scheme.
-- The table should contain an entry for each edge that is to be
-- translated /OR its reverse/ where by \"reverse\" we mean the
-- edge with the opposite ordering of vertices (e. g. @[3,1]@ vs.
-- @[1,3]@). The function already handles signs.
translateVia :: [(Simplex a, Symbol)] -> PreScheme a -> Scheme
translateVia tab pSch = map f pSch
    where
        f e = case lookup e tab of
                Just sym -> sym
                Nothing  -> invSymb . fromJust . lookup (reverse e) $ tab
