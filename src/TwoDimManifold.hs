{-# LANGUAGE LambdaCase #-}

module TwoDimManifold
(
      ComplexWL(..)
    , toComplWL
    , polygonScheme
    , polygonSchemeWL
    , polygonSchemeAtSimplex
    , identifySurface
    , buildSchemeWL
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
                       , SchemeWL(..)
                       , invSymb
                       , normalizeSchemeWL
 )

import Surface (
                   Surface
                 , identifySurfaceScheme
 )
 
import Util ( (.:) )


type PreScheme = [Simplex]

data ComplexWL = ComplexWL {  complex :: Complex
                            , mloop   :: Maybe PreScheme }
                 deriving (Show)

loop :: ComplexWL -> PreScheme
loop = fromMaybe [] . mloop

toComplWL :: Complex -> ComplexWL
toComplWL c = ComplexWL c Nothing


twoSimplexBoundary :: Simplex -> [Simplex]
twoSimplexBoundary s =
    [[s!!0,s!!1], [s!!1,s!!2], [s!!2,s!!0]]


identifySurface :: Complex -> Surface
identifySurface = identifySurfaceScheme . polygonScheme

polygonScheme :: Complex -> Scheme
polygonScheme =
    scheme . normalizeSchemeWL . polygonSchemeWL . toComplWL

polygonSchemeWL :: ComplexWL -> SchemeWL
polygonSchemeWL c =
    polygonSchemeAtSimplex c $ head $ delete [] $ complex c


polygonSchemeAtSimplex :: ComplexWL -> Simplex -> SchemeWL
polygonSchemeAtSimplex c s 
    | dimS s == 2   = polygonSchemeAtTwoSimplex c s
    | otherwise     = polygonSchemeAtTwoSimplex c s'
        where
            s' = head $ filter (isNSimplex 2) $ parentSimplices s (complex c)

polygonSchemeAtTwoSimplex :: ComplexWL -> Simplex -> SchemeWL
polygonSchemeAtTwoSimplex c s =
    buildSchemeWL (mloop c) . reverse $ dfsSimplices (complex c) s

buildScheme :: [Simplex] -> Scheme
buildScheme = scheme . buildSchemeWL Nothing

buildSchemeWL :: Maybe PreScheme -> [Simplex] -> SchemeWL
buildSchemeWL mbloop ss =
    fst $ buildSchemeWL' mbloop ss

buildSchemeWL' :: Maybe PreScheme -> [Simplex] -> (SchemeWL, [(Simplex,Symbol)])
buildSchemeWL' mbloop (s:ss) =
    (SchemeWL sch loop'', tab)
        where
            initPSch = twoSimplexBoundary s
            loop = fromMaybe [] mbloop
            (pSch, loop') = runState (buildPSchemeWL initPSch ss []) loop
            (sch, tab) = preSchemeToScheme pSch
            loop'' = mbloop >> Just (translateVia tab loop')

buildPSchemeWL :: PreScheme -> [Simplex] -> [Simplex] ->
                    State PreScheme PreScheme
buildPSchemeWL curPSch [] [] = return curPSch
buildPSchemeWL curPSch [] ss' = buildPSchemeWL curPSch ss' []
buildPSchemeWL curPSch (s:ss) ss' = do
    tryPasteSimplex s s curPSch >>= \case
        Nothing   -> buildPSchemeWL curPSch ss (s:ss')
        Just pSch -> buildPSchemeWL pSch ss ss'

tryPasteSimplex :: Simplex -> Simplex -> PreScheme -> 
                    State PreScheme (Maybe PreScheme)
tryPasteSimplex s s' sch =
    case break (`isFaceOf` s) sch of
      (_,[])            -> return Nothing
      (sch', (e:sch'')) -> do let [v1,v2] = e
                                  [v3] = s' \\ e
                              when (dimS s == 2) $ adjustLoop e s
                              return $ Just (sch' ++ [[v1,v3], [v3,v2]] ++ sch'')

adjustLoop :: Simplex -> Simplex -> State PreScheme ()
adjustLoop e s = do
    l' <- get >>= tryPasteSimplex e s
    unless (isNothing l') $ put (fromJust l')
    return ()

preSchemeToScheme :: PreScheme -> (Scheme, [(Simplex,Symbol)])
preSchemeToScheme pSch =
    (translateVia table pSch, table)
        where
            table = nubBy isFaceOf pSch `zip` ([toEnum 1 ..] :: [Symbol])

translateVia :: [(Simplex,Symbol)] -> PreScheme -> Scheme
translateVia tab pSch = map f pSch
    where
        f e = case lookup e tab of
                Just sym -> sym
                Nothing  -> invSymb . fromJust . lookup (reverse e) $ tab
