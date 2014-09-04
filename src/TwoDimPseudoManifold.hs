
module TwoDimPseudoManifold
(
      isTwoDimPseudoManifold
    , isTwoDimManifold
    , starComponents
    , isSingularity
    , singularities
    , fixSingularity
    , fixAllSingularities
--    , baseSurfaces
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

--import TwoDimManifold ( identifySurface )
--import Surface ( Surface )

import Util ( (.:) )


type StarComponent a = [Simplex a]
type StarComponents a = [StarComponent a]


isTwoDimPseudoManifold :: Complex a -> Bool
isTwoDimPseudoManifold c =
    dim c == 2 && isPseudoManifold c

isTwoDimManifold :: Complex a -> Bool
isTwoDimManifold c =
    isTwoDimPseudoManifold c && (null . singularities) c


starComponents :: Vertex a -> Complex a -> StarComponents a
starComponents = findComponents .: star

findComponents :: Complex a -> StarComponents a
findComponents c =
    case filter (isNSimplex 2) c of
        []  -> []
        s:_ -> let component = dfsSimplices c s
                   c' = c \\ component
               in component : findComponents c'

isSingleComponent :: StarComponents a -> Bool
isSingleComponent (_:[]) = True
isSingleComponent _      = False

isSingularity :: Vertex a -> Complex a -> Bool
isSingularity = not . isSingleComponent .: starComponents

singularities :: Complex a -> [(Vertex a, StarComponents a)]
singularities c =
    filter multipleComponents $ map vertexWithComponents $ vertices c
    where
        multipleComponents = not . isSingleComponent . snd
        vertexWithComponents v = (v, starComponents v c)

fixSingularity :: (Eq a) => Vertex a -> Complex a -> Complex (a, Int)
fixSingularity v c =
    let f  = id &&& const 0
        c' = complexMap f c
        v' = vMap f v
    in fixSingularity' v' c'
 
fixSingularity' :: (Eq a) => 
                    Vertex (a, Int) -> Complex (a, Int) -> Complex (a, Int)
fixSingularity' v c =
    case starComponents v c of
        _:[]  -> c
        comps -> fixSingularity'' v comps c

fixSingularity'' :: (Eq a) =>
                     Vertex (a, Int) -> StarComponents (a, Int) ->
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

--baseSurfaces :: Complex a -> [Surface]
--baseSurfaces =
--    map identifySurface . connectedComponents . fst . fixAllSingularities
