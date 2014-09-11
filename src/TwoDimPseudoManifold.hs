
-- |
-- Module      : TwoDimPseudoManifold
-- Description : Deal with two-dimensional pseudomanifolds
-- Copyright   : (c) Johannes Prem, 2014
-- License     : ISC License
-- 
-- Note that most of the functions in this module require their
-- input complex to be a 2-dimensional pseudomanifold, i. e.
-- 'isTwoDimPseudoManifold' would return 'True'. Do not call
-- those functions with other complexes (or bad things will
-- happen -- don't expect the world to be the same afterwards :D).

module TwoDimPseudoManifold
(
      isTwoDimPseudoManifold
    , isTwoDimManifold
    , isSingularity
    , singularities
    , fixSingularity
    , fixAllSingularities
    , baseSurfaces
) where

import Control.Arrow ( (&&&), second )
import Data.List (   
                     (\\)
                   , delete
                   , dropWhile
                   , foldr
                   , iterate
                   , union
 )

import SimplicialComplex (
                             Vertex(..)
                           , Simplex
                           , Complex
                           , complexMap
                           , connectedComponents
                           , dfsSimplices
                           , dim
                           , generatedBy
                           , isNSimplex
                           , isPseudoManifold
                           , parentSimplices
                           , star
                           , vertices
                           , vMap
 )

import TwoDimManifold ( identifySurface )
import Surface ( Surface )

import Util ( (.:) )


type StarSummand a = [Simplex a]
type StarSummands a = [StarSummand a]


-- | Test whether the input complex is a
--   pseudomanifold (see 'isPseudoManifold' in "SimplicialComplex")
--   of dimension @2@.
isTwoDimPseudoManifold :: Complex a -> Bool
isTwoDimPseudoManifold c =
    dim c == 2 && isPseudoManifold c

-- | Test whether the input complex triangulates
--   a closed surface.
isTwoDimManifold :: Complex a -> Bool
isTwoDimManifold c =
    isTwoDimPseudoManifold c && (null . singularities) c


-- Determine the wedge summands of the star of the input vertex.
--
-- The input complex must be a 2-dim. pseudomanifold.
starSummands :: Vertex a -> Complex a -> StarSummands a
starSummands = findSummands .: star

-- recursively find the wedge summands of a star; note that
-- the input complex is /not/ the full complex here but only
-- the star of the vertex.
findSummands :: Complex a -> StarSummands a
findSummands st =
    case filter (isNSimplex 2) st of
        []  -> []
        s:_ -> let summand = dfsSimplices st s
                   st' = st \\ summand
               in summand : findSummands st'

isSingleSummand :: StarSummands a -> Bool
isSingleSummand (_:[]) = True
isSingleSummand _      = False

-- | Test whether a given vertex is a singularity.
-- 
--   The input complex must be a 2-dim. pseudomanifold.
isSingularity :: Vertex a -> Complex a -> Bool
isSingularity = not . isSingleSummand .: starSummands

-- | Return all singularities of the input pseudomanifold
--   together with the corresponding wedge summands of the star.
--
--   The input complex must be a 2-dim. pseudomanifold.
singularities :: Complex a -> [(Vertex a, StarSummands a)]
singularities c =
    filter multipleSummands $ map vertexWithSummands $ vertices c
    where
        multipleSummands = not . isSingleSummand . snd
        vertexWithSummands v = (v, starSummands v c)

-- | Takes a vertex of a pseudomanifold and attempts to fix
--   the singularity at that point. Let @c'@ be an output complex
--   of this function. The second component @j@ of a vertex @(v,j)@
--   of @c'@ contains the following information:
--   * @j == 0@ means \"untouched\", i. e. there is exactly one
--     vertex in the output complex that corresponds to @v@,
--     namely @(v,0)@.
--   * @j /= 0@ means that the original vertex @v@ has been split
--     into multiple copies, one of which is the current vertex
--     @(v,j)@.
--   So the expression
--   
--   > filter ((== v) . vMap fst) $ vertices c'
--   
--   should either return only @[(v,0)]@ or a list of length at least
--   two.
--   
--   The input complex must be a 2-dim. pseudomanifold.
fixSingularity :: (Eq a) => Vertex a -> Complex a -> Complex (a, Int)
fixSingularity v c =
    let f  = id &&& const 0
        c' = complexMap f c
        v' = vMap f v
    in fixSingularity' v' c'
 
-- 'fixSingularity' actually only lifts a complex from type @a@
 -- to @(a,Int)@. 'fixSingularity'' and 'fixSingularity''' do the
 -- real work.
fixSingularity' :: (Eq a) => 
                    Vertex (a, Int) -> Complex (a, Int) -> Complex (a, Int)
fixSingularity' v c =
    case starSummands v c of
        _:[]  -> c
        sSs   -> fixSingularity'' v sSs c

-- see 'fixSingularity'' above
fixSingularity'' :: (Eq a) =>
                     Vertex (a, Int) -> StarSummands (a, Int) ->
                        Complex (a, Int) -> Complex (a, Int)
fixSingularity'' v sSs c =
    let sSs' = map (parentSimplices [v] . generatedBy) sSs
        oldSimplices = [v] : concatMap (delete [v]) sSs'
        newSimplices = concatMap (replaceStarSummand v) $ [1..] `zip` sSs'
    in (c \\ oldSimplices) `union` newSimplices

-- helper function that replaces vertices in a wedge summand
replaceStarSummand :: (Eq a) => 
                        Vertex (a, Int) -> (Int, [Simplex (a, Int)]) ->
                            [Simplex (a, Int)]
replaceStarSummand v (j, sS') =
    map (map subsv) sS'
        where
            vnew = vMap (second $ const j) v
            subsv vv | vv == v    =  vnew
                     | otherwise  =  vv

-- | Takes a pseudomanifold and fixes all singularities.
--   See 'fixSingularity' for a discussion about the output complex's
--   type.
--
--   The input complex must be a 2-dim. pseudomanifold.
fixAllSingularities :: (Eq a) => Complex a -> Complex (a, Int)
fixAllSingularities c =
    foldr fixSingularity' c' $ vertices c'
        where
            c' = complexMap (id &&& const 0) c

-- | Determines the closed surfaces that can be glued together
--   to obtain the input pseudomanifold.
baseSurfaces :: (Eq a) => Complex a -> [Surface]
baseSurfaces =
    map identifySurface . connectedComponents . fixAllSingularities
