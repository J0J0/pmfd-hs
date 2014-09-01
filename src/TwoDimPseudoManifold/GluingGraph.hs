
module TwoDimPseudoManifold.GluingGraph
(
      GluingGraph
    , GluedComplexes
    , GluedSurfaces
    , gluingGraph
    , gluingGraphSurf
) where
    
import Control.Arrow ( second )
import qualified Data.Map.Strict as  M
import qualified Data.Map.Lazy   as LM

import SimplicialComplex ( 
                             Complex
                           , connectedComponents
                           , vertexToIntegral
                           , vertices
 )
import TwoDimPseudoManifold ( fixAllSingularities )
import TwoDimManifold ( identifySurface )
import Surface ( Surface )


type GluingGraph i = M.Map (i,i) i
type GluedObj o i = LM.Map i o
type GluedComplexes i = GluedObj Complex i
type GluedSurfaces  i = GluedObj Surface i


gluingGraph :: (Integral i) =>
    Complex -> (GluingGraph i, GluedComplexes i)
gluingGraph = uncurry fixedGluingGraph . fixAllSingularities

fixedGluingGraph :: (Integral i) =>
    Complex -> i -> (GluingGraph i, GluedComplexes i)
fixedGluingGraph c offs =
    (graph, comps)
    where
        comps = LM.fromAscList $ [0..] `zip` connectedComponents c
        graph = LM.foldrWithKey (addGluingData offs) M.empty comps

addGluingData :: (Integral i) =>
    i -> i -> Complex -> GluingGraph i -> GluingGraph i
addGluingData offs j comp m =
    foldr (\ v -> M.insertWith (+) (v,j) 1) m gluedToVs
        where
            gluedVs = filter (>= offs) . map vertexToIntegral $ vertices comp
            gluedToVs = map (`mod` offs) gluedVs

gluingGraphSurf :: (Integral i) =>
    Complex -> (GluingGraph i, GluedSurfaces i)
gluingGraphSurf = identifyGluedSurfaces . gluingGraph

identifyGluedSurfaces :: (Integral i) =>
    (GluingGraph i, GluedComplexes i) -> (GluingGraph i, GluedSurfaces i)
identifyGluedSurfaces = second (LM.map identifySurface)
