
module SimplicialComplex.Generator where

import Data.List (
                     (\\)
                   , nub
                   , foldl
                   , foldr1
                   , sort
                   , union
 )
import qualified Data.Map.Strict as M

import SimplicialComplex
import Util ( (.:), both )


type GlueData = [(Vertex,Vertex)]

glueVertices :: GlueData -> Complex -> Complex
glueVertices glueData c =
    nub $ map (nub . sort . map projectVertex) c
        where
            glueMap = M.fromAscList glueData
            projectVertex v = M.findWithDefault v v glueMap

triangulatedRectangle :: Int -> Int -> Complex
triangulatedRectangle nx ny = triangulatedRectangle' nx ny []

triangulatedRectangle' :: Int -> Int -> [(Int,Int)] -> Complex
triangulatedRectangle' nx ny swapped =
    generatedBy $ map (map $ lexiV nx) $ concat [
        [[(x,y),(x+1,y),v1],
         [v2,(x,y+1),(x+1,y+1)]] | y <- [1..ny], x <- [1..nx],
                                   let (v1,v2) = sw x y
    ]
        where
            sw x y = if (x,y) `elem` swapped
                     then ( (x,y+1), (x+1,y) )
                     else ( (x+1,y+1), (x,y) )

lexiV :: Int -> (Int, Int) -> Vertex
lexiV nx (x,y) = vertexFromIntegral $ x + (nx+1)*(y-1)

data GlueOr   = SameOr | OppOr  deriving (Eq, Ord, Enum, Show)
data GlueSpec = GlueSpec { xOr :: GlueOr, yOr :: GlueOr }  deriving (Eq, Show)

gluedRectangle :: GlueSpec -> Complex
gluedRectangle = gluedRectangle' 3 3

gluedRectangle' :: Int -> Int -> GlueSpec -> Complex
gluedRectangle' nx ny gs =
    glueVertices glueData $ triangulatedRectangle' nx ny [(nx,1)]
        where
            glueData = fromIntData $ genRectGlueIData nx ny gs

genRectGlueIData :: Int -> Int -> GlueSpec -> [(Int,Int)]
genRectGlueIData nx ny gs =
    let mx = nx+1
        my = ny+1
        t  = case gs of GlueSpec OppOr OppOr -> mx
                        _                    -> 1
        trafo f = case f gs of SameOr -> id
                               OppOr  -> reverse
    in
        [(mx, t)]                                                 ++
        [2*mx,3*mx..mx*ny] `zip` trafo yOr [mx+1,2*mx+1..mx*ny]   ++
        [(mx*ny+1, t)]                                            ++
        [mx*ny+2..mx*my-1] `zip` trafo xOr [2..mx-1]              ++
        [(mx*my, 1)]

fromIntData :: [(Int,Int)] -> GlueData
fromIntData = map (both vertexFromIntegral)


disjointUnion :: Complex -> Complex -> Complex
disjointUnion = fst .: disjointUnion'

disjointUnion' :: (Integral i) => Complex -> Complex -> (Complex, i)
disjointUnion' c1 c2 =
    (c1 `union` c2', offs)
        where
            c2'  = complexMapCalc (+offs) c2
            offs = calculateOffset c1

disjointUnions :: [Complex] -> Complex
disjointUnions = foldr1 disjointUnion


connectedSumAt :: Simplex -> Complex -> Simplex -> Complex -> Complex
connectedSumAt s1 c1 s2 c2 =
    let (cc, offs) = disjointUnion' c1 c2
        s2' = simplexMapCalc (+offs) s2
        gd = s2' `zip` s1
        cc' = if dimS s1 == 0 then cc else cc \\ [s1,s2']
    in glueVertices gd $ cc'

connectedSum :: Complex -> Complex -> Complex
connectedSum c1 c2 =
    let d = min (dim c1) (dim c2)
    in connectedSum' d c1 c2

connectedSum' :: (Integral i) => i -> Complex -> Complex -> Complex
connectedSum' d c1 c2 =
    let (s1, s2) = both findDSimplex (c1, c2)
        findDSimplex = head . filter (isNSimplex d) 
    in connectedSumAt s1 c1 s2 c2

connectedSums :: [Complex] -> Complex
connectedSums = foldr1 connectedSum


wedgeSumAt :: Vertex -> Complex -> Vertex -> Complex -> Complex
wedgeSumAt v1 c1 v2 c2 =
    connectedSumAt [v1] c1 [v2] c2

wedgeSum :: Complex -> Complex -> Complex
wedgeSum = connectedSum' 0

wedgeSums :: [Complex] -> Complex
wedgeSums (c:cs) =
    foldl (\ cc c' -> wedgeSumAt v0 cc (anyV c') c') c cs
        where
            v0 = head $ vertices c
            anyV c' = head $ vertices c'
