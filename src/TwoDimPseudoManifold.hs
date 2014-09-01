
module TwoDimPseudoManifold
(
      isTwoDimPseudoManifold
    , isTwoDimManifold
    , starComponents
    , isSingularity
    , singularities
    , fixSingularity
    , fixAllSingularities
    , baseSurfaces
) where

import Data.List (   
                     (\\)
                   , delete
                   , dropWhile
                   , foldr
                   , iterate
                   , union
 )

import SimplicialComplex (
                             Vertex
                           , Simplex
                           , Complex
                           , calculateOffset
                           , connectedComponents
                           , dfsSimplices
                           , dim
                           , generatedBy
                           , isNSimplex
                           , isPseudoManifold
                           , parentSimplices
                           , star
                           , vertexToIntegral
                           , vertexFromIntegral
                           , vertexMapCalc
                           , vertices
 )

import TwoDimManifold ( identifySurface )
import Surface ( Surface )

import Util ( (.:) )


type StarComponent = [Simplex]
type StarComponents = [StarComponent]


isTwoDimPseudoManifold :: Complex -> Bool
isTwoDimPseudoManifold c =
    dim c == 2 && isPseudoManifold c

isTwoDimManifold :: Complex -> Bool
isTwoDimManifold c =
    isTwoDimPseudoManifold c && (null . singularities) c


starComponents :: Vertex -> Complex -> StarComponents
starComponents = findComponents .: star

findComponents :: Complex -> StarComponents
findComponents c =
    case filter (isNSimplex 2) c of
        []  -> []
        s:_ -> let component = dfsSimplices c s
                   c' = c \\ component
               in component : findComponents c'

isSingleComponent :: StarComponents -> Bool
isSingleComponent (_:[]) = True
isSingleComponent _      = False

isSingularity :: Vertex -> Complex -> Bool
isSingularity = not . isSingleComponent .: starComponents

singularities :: Complex -> [(Vertex, StarComponents)]
singularities c =
    filter multipleComponents $ map vertexWithComponents $ vertices c
    where
        multipleComponents = not . isSingleComponent . snd
        vertexWithComponents v = (v, starComponents v c)

fixSingularity :: (Vertex,StarComponents) -> Complex -> Complex
fixSingularity sing c =
    fixSingularityOffset (calculateOffset c) sing c

fixSingularityOffset :: (Integral i) =>
    i -> (Vertex,StarComponents) -> Complex -> Complex
fixSingularityOffset offs (v, comps) c =
    foldr (replaceComp offs v) c $ zip [1..] comps

replaceComp :: (Integral i) =>
    i -> Vertex -> (i, StarComponent) -> Complex -> Complex
replaceComp offs v (j, comp) c =
    (c \\ oldSimplices) `union` newSimplices
        where
            oldSimplices = parentSimplices [v] $ generatedBy comp
            vnew = vertexMapCalc (\ k -> j*offs + k) v
            newSimplices = map ((vnew:) . (delete v)) oldSimplices

fixAllSingularities :: (Integral i) => Complex -> (Complex, i)
fixAllSingularities c =
    flip (,) offs $
        foldr (fixSingularityIfNecessary offs) c $ vertices c
            where
                offs = calculateOffset c
                fixSingularityIfNecessary o v c' =
                    case starComponents v c' of
                        _:[]   -> c'
                        comps  -> fixSingularityOffset o (v, comps) c'

baseSurfaces :: Complex -> [Surface]
baseSurfaces =
    map identifySurface . connectedComponents . fst . fixAllSingularities
