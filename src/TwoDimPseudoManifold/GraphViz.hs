{-# LANGUAGE StandaloneDeriving #-}

module TwoDimPseudoManifold.GraphViz
(
      writeGluingGraph
    , writeGluingGraph'
    , visualizeGluingGraph
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
                                            GluingGraphD
                                          , GluedSurfaces
 )
import Surface ( Surface )


data NodeType = GluingNode Int | SurfaceNode Int deriving (Eq, Ord, Show)

instance GV.PrintDot (NodeType) where
    unqtDot (GluingNode x)  = (PP.<>) (PP.char 'g') (GV.unqtDot x)
    unqtDot (SurfaceNode x) = (PP.<>) (PP.char 's') (GV.unqtDot x)

type Node = (NodeType, Maybe Surface)
type Edge = (NodeType, NodeType, Int)

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

clusterize :: Node -> GV.NodeCluster Cluster Node
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

nodeFmt :: Node -> GV.Attributes
nodeFmt (GluingNode _, _)       = []
nodeFmt (SurfaceNode _, Just s) = [ GV.toLabel $ show s ]

writeGluingGraph :: (GluingGraphD, GluedSurfaces) -> FilePath -> IO ()
writeGluingGraph = flip writeGluingGraph' $ GV.Png

writeGluingGraph' :: (GluingGraphD, GluedSurfaces) ->
                         GV.GraphvizOutput -> FilePath -> IO ()
writeGluingGraph' gd gvo path =
    runGraphviz (\ dotg -> GV.runGraphviz dotg gvo path) gd

visualizeGluingGraph :: (GluingGraphD, GluedSurfaces) -> IO ()
visualizeGluingGraph gd =
    runGraphviz (flip GV.runGraphvizCanvas' GV.Xlib) gd

runGraphviz :: (GV.DotGraph NodeType -> IO a) ->
                  (GluingGraphD, GluedSurfaces) -> IO ()
runGraphviz f gd@(gg, _) =
    f dg >> return ()
        where
            ns = buildNodes gd
            es = buildEdges gg
            dg = GV.graphElemsToDot gvParams ns es


buildNodes :: (GluingGraphD, GluedSurfaces) -> [Node]
buildNodes (m, mm) =
    Set.toList gluingNodes ++ surfaceNodes
        where
            edges = M.keysSet m
            gluingNodes  = Set.map (GluingNode *** const Nothing) edges
            surfaceNodes = map (SurfaceNode *** Just) $ LM.toList mm

buildEdges :: GluingGraphD -> [Edge]
buildEdges m =
    M.foldrWithKey addEdges [] m
        where
            addEdges (v,j) t = ([ (gn v, sn j, l) | l <- [1..t] ] ++)
            gn = GluingNode
            sn = SurfaceNode
