{-# LANGUAGE LambdaCase, StandaloneDeriving #-}

module TwoDimManifold
(
      ComplexWL(..)
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


type PreScheme a = [Simplex a]

data ComplexWL a = ComplexWL {   complex :: Complex a
                               , cloop   :: Maybe (PreScheme a) }

deriving instance (Show a) => Show (ComplexWL a)

loop :: ComplexWL a -> PreScheme a
loop = fromMaybe [] . cloop

toComplWL :: Complex a -> ComplexWL a
toComplWL c = ComplexWL c Nothing


twoSimplexBoundary :: Simplex a -> [Simplex a]
twoSimplexBoundary s =
    [[s!!0,s!!1], [s!!1,s!!2], [s!!2,s!!0]]


identifySurface :: Complex a -> Surface
identifySurface = identifySurfaceScheme . polygonScheme


-- FIXME: the whole polygonScheme stuff needs a refactoring:
--        conversion from PreScheme to Scheme should happen
--        much later and, optimally, PreScheme/Scheme would be
--        clearly separated while both should be exported since we
--        need direct access to PreSchemes in TwoDimPseudoManifold.Loop.
--        'polygonPSchemeWL' is just a quick hack used in the latter
--        module ... :(
 
polygonScheme :: Complex a -> Scheme
polygonScheme =
    scheme . normalizeSchemeWL . polygonSchemeWL . toComplWL

polygonSchemeWL :: ComplexWL a -> SchemeWL
polygonSchemeWL c =
    polygonSchemeAtSimplex c $ head $ delete [] $ complex c

-- meh, duplicate code, see FIXME above ...
polygonPSchemeWL :: Complex a -> PreScheme a -> (PreScheme a, PreScheme a)
polygonPSchemeWL c loop =
    runState (buildPSchemeWL initPSch ss []) loop
        where
            s'     = head $ filter (isNSimplex 2) c
            (s:ss) = reverse $ dfsSimplices c s'
            initPSch = twoSimplexBoundary s


polygonSchemeAtSimplex :: ComplexWL a -> Simplex a -> SchemeWL
polygonSchemeAtSimplex c s 
    | dimS s == 2   = polygonSchemeAtTwoSimplex c s
    | otherwise     = polygonSchemeAtTwoSimplex c s'
        where
            s' = head $ filter (isNSimplex 2) $ parentSimplices s (complex c)

polygonSchemeAtTwoSimplex :: ComplexWL a -> Simplex a -> SchemeWL
polygonSchemeAtTwoSimplex c s =
    buildSchemeWL (cloop c) . reverse $ dfsSimplices (complex c) s

buildScheme :: [Simplex a] -> Scheme
buildScheme = scheme . buildSchemeWL Nothing

buildSchemeWL :: Maybe (PreScheme a) -> [Simplex a] -> SchemeWL
buildSchemeWL mbloop ss =
    fst $ buildSchemeWL' mbloop ss

buildSchemeWL' :: Maybe (PreScheme a) -> [Simplex a] -> (SchemeWL, [(Simplex a, Symbol)])
buildSchemeWL' mbloop (s:ss) =
    (SchemeWL sch loop'', tab)
        where
            initPSch = twoSimplexBoundary s
            loop = fromMaybe [] mbloop
            (pSch, loop') = runState (buildPSchemeWL initPSch ss []) loop
            (sch, tab) = preSchemeToScheme pSch
            loop'' = mbloop >> Just (translateVia tab loop')

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

tryPasteSimplex :: Simplex a -> Simplex a -> (PreScheme a) -> 
                    State (PreScheme a) (Maybe (PreScheme a))
tryPasteSimplex s s' sch =
    case break (`isFaceOf` s) sch of
      (_,[])            -> return Nothing
      (sch', (e:sch'')) -> do let [v1,v2] = e
                                  [v3] = s' \\ e
                              when (dimS s == 2) $ adjustLoop e s
                              return $ Just (sch' ++ [[v1,v3], [v3,v2]] ++ sch'')

adjustLoop :: Simplex a -> Simplex a -> State (PreScheme a) ()
adjustLoop e s = do
    get >>= tryPasteSimplex e s >>= \case
        Nothing -> return ()
        Just l' -> do put l'
                      adjustLoop e s

preSchemeToScheme :: PreScheme a -> (Scheme, [(Simplex a, Symbol)])
preSchemeToScheme = preSchemeToScheme' 1
    
preSchemeToScheme' :: Int -> PreScheme a -> (Scheme, [(Simplex a, Symbol)])
preSchemeToScheme' t pSch =
    (translateVia table pSch, table)
        where
            table = nubBy isFaceOf pSch `zip` ([toEnum t ..] :: [Symbol])

translateVia :: [(Simplex a, Symbol)] -> PreScheme a -> Scheme
translateVia tab pSch = map f pSch
    where
        f e = case lookup e tab of
                Just sym -> sym
                Nothing  -> invSymb . fromJust . lookup (reverse e) $ tab
