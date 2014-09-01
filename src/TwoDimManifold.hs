
module TwoDimManifold
(
      polygonScheme
    , polygonSchemeAtSimplex
    , identifySurface
) where

import Control.Arrow ( second )
import Data.List (   
                     (\\)
                   , delete
                   , foldr
                   , mapAccumR
                   , nubBy
 )
import Data.Maybe ( catMaybes, fromJust )

import SimplicialComplex (
                             Vertex
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
                       , invSymb
                       , normalizeScheme
 )

import Surface (
                   Surface
                 , identifySurfaceScheme
 )
 
import Util ( (.:) )


type PreScheme = [Simplex]


twoSimplexBoundary :: Simplex -> [Simplex]
twoSimplexBoundary s =
    [[s!!0,s!!1], [s!!1,s!!2], [s!!2,s!!0]]


identifySurface :: Complex -> Surface
identifySurface = identifySurfaceScheme . normalizeScheme . polygonScheme


polygonSchemeNC :: Complex -> Scheme
polygonSchemeNC c =
    polygonSchemeAtSimplexNC c $ head $ delete [] c

polygonScheme :: Complex -> Scheme
polygonScheme = normalizeScheme . polygonSchemeNC

polygonSchemeAtSimplexNC :: Complex -> Simplex -> Scheme
polygonSchemeAtSimplexNC c s 
    | dimS s == 2   = polygonSchemeAtTwoSimplexNC c s
    | otherwise     = polygonSchemeAtTwoSimplexNC c s'
        where
            s' = head $ filter (isNSimplex 2) $ parentSimplices s c

polygonSchemeAtSimplex :: Complex -> Simplex -> Scheme
polygonSchemeAtSimplex = normalizeScheme .: polygonSchemeAtSimplexNC

polygonSchemeAtTwoSimplexNC :: Complex -> Simplex -> Scheme
polygonSchemeAtTwoSimplexNC = buildScheme .: dfsSimplices

buildScheme :: [Simplex] -> Scheme
buildScheme (s:ss) =
    preSchemeToScheme $ buildPSchemeInit initPSch ss
        where
            initPSch = twoSimplexBoundary s

buildPSchemeInit :: [Simplex] -> [Simplex] -> PreScheme
buildPSchemeInit initPSch [] = initPSch
buildPSchemeInit initPSch ss =
     uncurry buildPSchemeInit $ 
         second catMaybes $ mapAccumR tryPasteSimplex initPSch ss

tryPasteSimplex :: PreScheme -> Simplex -> (PreScheme, Maybe Simplex)
tryPasteSimplex sch s =
    case break (`isFaceOf` s) sch of
      (_,[])            -> (sch, Just s)
      (sch', (e:sch'')) -> let [v1,v2] = e
                               [v3] = s \\ e
                           in (sch' ++ [[v1,v3], [v3,v2]] ++ sch'', Nothing)

preSchemeToScheme :: PreScheme -> Scheme
preSchemeToScheme sch =
    map (translateVia table) sch
        where
            table = nubBy isFaceOf sch `zip` ([toEnum 1 ..] :: [Symbol])
            translateVia tab e =
                case lookup e tab of
                    Just sym -> sym
                    Nothing  -> invSymb . fromJust . lookup (reverse e) $ tab
