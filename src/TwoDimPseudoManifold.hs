
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
findSummands c =
    case filter (isNSimplex 2) c of
        []  -> []
        s:_ -> let summand = dfsSimplices c s
                   c' = c \\ summand
               in summand : findSummands c'

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
        comps -> fixSingularity'' v comps c

fixSingularity'' :: (Eq a) =>
                     Vertex (a, Int) -> StarSummands (a, Int) ->
                        Complex (a, Int) -> Complex (a, Int)
fixSingularity'' v comps c =
    let comps' = map (parentSimplices [v] . generatedBy) comps
        oldSimplices = [v] : concatMap (delete [v]) comps'
        newSimplices = concatMap (replaceComp v) $ [1..] `zip` comps'
    in (c \\ oldSimplices) `union` newSimplices

replaceComp :: (Eq a) => 
                Vertex (a, Int) -> (Int, [Simplex (a, Int)]) ->
                    [Simplex (a, Int)]
replaceComp v (j, comp') =
    map (map subsv) comp'
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
