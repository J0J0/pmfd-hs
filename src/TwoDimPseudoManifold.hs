
module TwoDimPseudoManifold
(
      isTwoDimPseudoManifold
    , isTwoDimManifold
    , starSummands
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


isTwoDimPseudoManifold :: Complex a -> Bool
isTwoDimPseudoManifold c =
    dim c == 2 && isPseudoManifold c

isTwoDimManifold :: Complex a -> Bool
isTwoDimManifold c =
    isTwoDimPseudoManifold c && (null . singularities) c


starSummands :: Vertex a -> Complex a -> StarSummands a
starSummands = findSummands .: star

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

isSingularity :: Vertex a -> Complex a -> Bool
isSingularity = not . isSingleSummand .: starSummands

singularities :: Complex a -> [(Vertex a, StarSummands a)]
singularities c =
    filter multipleSummands $ map vertexWithSummands $ vertices c
    where
        multipleSummands = not . isSingleSummand . snd
        vertexWithSummands v = (v, starSummands v c)

fixSingularity :: (Eq a) => Vertex a -> Complex a -> Complex (a, Int)
fixSingularity v c =
    let f  = id &&& const 0
        c' = complexMap f c
        v' = vMap f v
    in fixSingularity' v' c'
 
fixSingularity' :: (Eq a) => 
                    Vertex (a, Int) -> Complex (a, Int) -> Complex (a, Int)
fixSingularity' v c =
    case starSummands v c of
        _:[]  -> c
        sSs   -> fixSingularity'' v sSs c

fixSingularity'' :: (Eq a) =>
                     Vertex (a, Int) -> StarSummands (a, Int) ->
                        Complex (a, Int) -> Complex (a, Int)
fixSingularity'' v sSs c =
    let sSs' = map (parentSimplices [v] . generatedBy) sSs
        oldSimplices = [v] : concatMap (delete [v]) sSs'
        newSimplices = concatMap (replaceStarSummand v) $ [1..] `zip` sSs'
    in (c \\ oldSimplices) `union` newSimplices

replaceStarSummand :: (Eq a) => 
                        Vertex (a, Int) -> (Int, [Simplex (a, Int)]) ->
                            [Simplex (a, Int)]
replaceStarSummand v (j, sS') =
    map (map subsv) sS'
        where
            vnew = vMap (second $ const j) v
            subsv vv | vv == v    =  vnew
                     | otherwise  =  vv

fixAllSingularities :: (Eq a) => Complex a -> Complex (a, Int)
fixAllSingularities c =
    foldr fixSingularity' c' $ vertices c'
        where
            c' = complexMap (id &&& const 0) c

baseSurfaces :: (Eq a) => Complex a -> [Surface]
baseSurfaces =
    map identifySurface . connectedComponents . fixAllSingularities
