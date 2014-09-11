{-# LANGUAGE StandaloneDeriving #-}

-- |
-- Module      : TwoDimPseudoManifold.GluingGraph
-- Description : A graph representation of a 2-dim. pseudomanifold
-- Copyright   : (c) Johannes Prem, 2014
-- License     : ISC License

module TwoDimPseudoManifold.GluingGraph
(
      GComplex
    , GluingGraphD
    , GluedObj
    , GluedVertices
    , GluedComplexes
    , GluedSurfaces
    , GluedD(..)
    , gluingGraph
    , gluingGraphSurf
    , gluingGraphFromFixed
    , identifyGluedSurfaces
) where
    
import Control.Arrow ( (&&&), second )
import Data.List ( nub )
import qualified Data.Map.Strict as  M
import qualified Data.Map.Lazy   as LM
import Data.Maybe ( fromJust )
import Data.Tuple ( swap )

import SimplicialComplex ( 
                             Vertex(..)
                           , Complex
                           , connectedComponents
                           , vertices
                           , vMap
 )
import TwoDimPseudoManifold ( fixAllSingularities )
import TwoDimManifold ( identifySurface )
import Surface ( Surface )


type GComplex a   = Complex (a, Int)
type GluingGraphD = M.Map (Int,Int) Int
type GluedObj o   = LM.Map Int o
type GluedVertices  a = GluedObj (Vertex a)
type GluedComplexes a = GluedObj (GComplex a)
type GluedSurfaces    = GluedObj Surface

-- | A 'GluedD' holds the graph data that we extract
--   from a 2-dim. pseudomanifold.
data GluedD a = GluedD {  
                         glGraphD    :: GluingGraphD
                       , glVertices  :: GluedVertices a
                       , glComplexes :: GluedComplexes a
                       }

deriving instance (Show a) => Show (GluedD a)

-- | 'gluingGraph' takes a 2-dim. pseudomanifold,
--   fixes all singularities and extracts the gluing
--   information.
--   The input complex must be a 2-dim. pseudomanifold.
gluingGraph :: (Eq a) =>  Complex a -> GluedD a
gluingGraph = gluingGraphFromFixed . fixAllSingularities

-- | 'gluingGraphFromFixed' takes a 2-dim. manifold of the
--   form that 'fixAllSingularities' returns and extracts
--   the gluing information.
--   
--   The input complex must be a 2-dim. manifold of the type
--   and format described by 'fixSingularity' in "TwoDimPseudoManifold".
gluingGraphFromFixed :: (Eq a) => GComplex a -> GluedD a
gluingGraphFromFixed c =
    GluedD { glGraphD = graph, glVertices = vsm, glComplexes = comps }
        where
            comps = LM.fromDistinctAscList $ [0..] `zip` connectedComponents c
            vs  = nub $ map (vMap fst) $ filter isGluedV $ vertices c
            vsi = vs `zip` [0..]
            vsm = LM.fromDistinctAscList $ map swap vsi
            graph = LM.foldrWithKey (addGluingData vsi) M.empty comps

isGluedV :: Vertex (a, Int) -> Bool
isGluedV (Vertex (_,t)) = t /= 0

addGluingData :: (Eq a) =>
                    [(Vertex a, Int)] -> Int -> GComplex a ->
                        GluingGraphD -> GluingGraphD
addGluingData vsi j comp m =
    foldr (\ v -> M.insertWith (+) (toId v,j) 1) m gluedToVs
        where
            gluedVs   = filter isGluedV $ vertices comp
            gluedToVs = map (vMap fst) gluedVs 
            toId v = fromJust $ lookup v vsi

-- | Convenience function that obtains the 'GlueD' for a 2-dim.
--   pseudomanifold and extracts the most interesting parts,
--   namely the actual graph data and which surfaces are glued
--   together.
--   
--   The input complex must be a 2-dim. pseudomanifold.
gluingGraphSurf :: (Eq a) =>
                    Complex a -> (GluingGraphD, GluedSurfaces)
gluingGraphSurf =
    (glGraphD &&& identifyGluedSurfaces) . gluingGraph

identifyGluedSurfaces :: GluedD a -> GluedSurfaces
identifyGluedSurfaces = LM.map identifySurface . glComplexes
