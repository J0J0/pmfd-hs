
module SimplicialComplex
(
      Vertex
    , Simplex
    , Complex
    , vertexToIntegral
    , vertexFromIntegral
    , vertexMapCalc
    , dimS
    , isNSimplex
    , isFaceOf
    , simplexMapCalc
    , vertices
    , dim
    , generatedBy
    , facets
    , connectedComponents
    , calculateOffset
    , complexMapCalc
    , isValid
    , isConnected
    , isPseudoManifold
    , isStronglyConnected
    , parentSimplices
    , star
    , eulerCharacteristic
    , dfsVertices
    , dfsSimplices
) where

import Data.List (
                    (\\) 
                   , concatMap
                   , delete
                   , foldl'
                   , foldr
                   , genericLength
                   , intersect
                   , maximum
                   , subsequences
                   , unfoldr
                   , union
 )
import Control.Monad.State
import Util ( equalLength, roundToMagnitude )

newtype Vertex = Vertex { val :: Integer } deriving (Eq, Ord)
type Simplex = [Vertex]
type Complex = [Simplex]


-- vertices

instance Show Vertex where
    show = show . val

vertexToIntegral :: (Integral i) => Vertex -> i
vertexToIntegral = fromIntegral . val

vertexFromIntegral :: (Integral i) => i -> Vertex
vertexFromIntegral = Vertex . fromIntegral

vertexMapCalc :: (Integral i) => (i -> i) -> Vertex -> Vertex
vertexMapCalc f = vertexFromIntegral . f . vertexToIntegral


-- simplices

dimS :: (Integral i) => Simplex -> i
dimS s = genericLength s - 1

isNSimplex :: (Integral i) => i -> Simplex -> Bool
isNSimplex n s = dimS s == n

isFaceOf :: Simplex -> Simplex -> Bool
isFaceOf s s' = all (`elem` s') s

simplexMapCalc :: (Integral i) => (i -> i) -> Simplex -> Simplex
simplexMapCalc f = map (vertexMapCalc f)


-- complexes

vertices :: Complex -> [Vertex]
vertices = foldr union []

dim :: (Integral i) => Complex -> i
dim = maximum . map dimS

facets :: Complex -> [Simplex]
facets c = filter (isNSimplex $ dim c) c

generatedBy :: [Simplex] -> Complex
generatedBy c = foldr addFaces c c
    where
        addFaces s c' = c' `union` (subsequences s)

calculateOffset :: (Integral i) => Complex -> i
calculateOffset =
    roundToMagnitude . (+1) . maximum . map vertexToIntegral . vertices

complexMapCalc :: (Integral i) => (i -> i) -> Complex -> Complex
complexMapCalc f = map (simplexMapCalc f)

isValid :: Complex -> Bool
isValid c = all (containsFaces c) c
    where
        containsFaces c' s = all (`elem` c') $ subsequences s

isConnected :: Complex -> Bool
isConnected c = equalLength allVertices foundVertices
    where
        allVertices = vertices c
        foundVertices = dfsVertices c $ head allVertices

isPseudoManifold :: Complex -> Bool
isPseudoManifold c = isNPseudoManifold (dim c) c

isNPseudoManifold :: (Integral i) => i -> Complex -> Bool
isNPseudoManifold n c = all (validSimplex n c) c
    where
        validSimplex n c' s =
            let parentFacets = filter (isNSimplex n) $ parentSimplices s c'
            in case compare (dimS s) (n-1) of
                GT -> True
                EQ -> length parentFacets == 2
                LT -> not . null $ parentFacets

isStronglyConnected :: Complex -> Bool
isStronglyConnected c = isNStronglyConnected (dim c) c

isNStronglyConnected :: (Integral i) => i -> Complex -> Bool
isNStronglyConnected n c = equalLength nSimplices foundSimplices
    where
        nSimplices = filter (isNSimplex n) c
        foundSimplices = dfsSimplices c $ head nSimplices

parentSimplices :: Simplex -> [Simplex] -> [Simplex]
parentSimplices s = filter (s `isFaceOf`)

star :: Vertex -> Complex -> Complex
star v c = generatedBy $ parentSimplices [v] c

eulerCharacteristic :: (Integral i) => Complex -> i
eulerCharacteristic =
    foldl' f 0 . delete []
        where
            f sum s = sum + (-1)^(dimS s)

connectedComponents :: Complex -> [Complex]
connectedComponents c = unfoldr (findConnectedComponent c) $ vertices c

findConnectedComponent :: Complex -> [Vertex] -> Maybe (Complex, [Vertex])
findConnectedComponent _ [] = Nothing
findConnectedComponent c vs =
    Just (component, remainingVertices)
        where
            vsComp = dfsVertices c $ head vs
            component = [] : filter (not . null . intersect vsComp) c
            remainingVertices = vs \\ vsComp


dfsVertices :: Complex -> Vertex -> [Vertex]
dfsVertices c v = map head $ execState (dfs dfsHVertex c [v]) []

dfsSimplices :: Complex -> Simplex -> [Simplex]
dfsSimplices c s = execState (dfs dfsHSimplex c s) []


-- private

mark :: Simplex -> State [Simplex] ()
mark s = modify (s:)
marked :: Simplex -> State [Simplex] Bool
marked s = gets (s `elem`)
dfs :: DfsHFunc -> Complex -> Simplex -> State [Simplex] ()
dfs adjObj c s = do
    visited <- marked s
    if visited
    then return ()
    else do
        mark s
        mapM_ (dfs adjObj c) $ adjObj s c

type DfsHFunc = Simplex -> Complex -> [Simplex]

dfsHVertex :: DfsHFunc
dfsHVertex s@[v] c =
    let edges = filter (isNSimplex 1) $ parentSimplices s c
    in map (delete v) edges

dfsHSimplex :: DfsHFunc
dfsHSimplex s c =
    let n = dimS s
        isAdjacend = isNSimplex (n-1) . (s `intersect`)
    in filter (\ s' -> isNSimplex n s' && isAdjacend s') c
