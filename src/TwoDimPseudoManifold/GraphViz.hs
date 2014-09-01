{-# LANGUAGE StandaloneDeriving #-}

module TwoDimPseudoManifold.GraphViz
(
    visualizeGluingGraph
) where

import Control.Arrow ( (***) )
import qualified Data.GraphViz as GV
import qualified Data.Map.Strict as  M
import qualified Data.Map.Lazy   as LM
import Data.Maybe ( isNothing )
import qualified Data.Set as Set
-- for use with GraphViz:
import qualified Text.PrettyPrint.Leijen.Text.Monadic as PP (char, (<>))

import TwoDimPseudoManifold.GluingGraph (
                                            GluingGraph
                                          , GluedSurfaces
 )
import Surface ( Surface )


data NodeType i = GluingNode i | SurfaceNode i deriving (Show)
deriving instance (Eq i)  => Eq  (NodeType i)
deriving instance (Ord i) => Ord (NodeType i)

instance (Integral i) => GV.PrintDot (NodeType i) where
    unqtDot (GluingNode x)  = (PP.<>) (PP.char 'g') (GV.unqtDot $ toInteger x)
    unqtDot (SurfaceNode x) = (PP.<>) (PP.char 's') (GV.unqtDot $ toInteger x)

type Node i = (NodeType i, Maybe Surface)
type Edge i = (NodeType i, NodeType i, i)

--gvParams :: GraphvizParams ...
gvParams = GV.Params {   GV.isDirected       = False
                       , GV.globalAttributes = []
                       , GV.clusterBy        = clusterize
                       , GV.isDotCluster     = const False
                       , GV.clusterID        = GV.Num . GV.Int . fromEnum
                       , GV.fmtCluster       = clusterFmt
                       , GV.fmtNode          = nodeFmt
                       , GV.fmtEdge          = const []
                     }

data Cluster = GluingCl | SurfaceCl deriving (Eq, Ord, Enum, Bounded, Show)

clusterize :: Node i -> GV.NodeCluster Cluster (Node i)
clusterize (n,s) =
    GV.C cl $ GV.N (n,s)
        where
            cl = if isNothing s then GluingCl else SurfaceCl

clusterFmt :: Cluster -> [GV.GlobalAttributes]
clusterFmt cl =
    [ GV.NodeAttrs [ GV.shape $ sh cl ] ]
        where
            sh GluingCl  = GV.PointShape
            sh SurfaceCl = GV.Ellipse

nodeFmt :: Node i -> GV.Attributes
nodeFmt (GluingNode _, _)       = []
nodeFmt (SurfaceNode _, Just s) = [ GV.toLabel $ show s ]

visualizeGluingGraph :: (Integral i) =>
    (GluingGraph i, GluedSurfaces i) -> IO ()
visualizeGluingGraph gd@(gg, _) =
    GV.runGraphvizCanvas' (GV.graphElemsToDot gvParams ns es) GV.Xlib
        where
            ns = buildNodes gd
            es = buildEdges gg

buildNodes :: (Integral i) => (GluingGraph i, GluedSurfaces i) -> [Node i]
buildNodes (m, mm) =
    Set.toList gluingNodes ++ surfaceNodes
        where
            edges = M.keysSet m
            gluingNodes  = Set.map (GluingNode *** const Nothing) edges
            surfaceNodes = map (SurfaceNode *** Just) $ LM.toList mm

buildEdges :: (Integral i) => GluingGraph i -> [Edge i]
buildEdges m =
    M.foldrWithKey addEdges [] m
        where
            addEdges (v,j) t = ([ (gn v, sn j, l) | l <- [1..t] ] ++)
            gn = GluingNode
            sn = SurfaceNode
