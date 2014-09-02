
-- |
-- Module      : SimplicialComplex
-- Description : Simplicial complexes
-- Copyright   : (c) Johannes Prem, 2014
-- License     : ISC License
-- 
-- This module contains the basic 'Complex' type, used throughout
-- the software. We don't aim for efficiency but for a representation
-- that is closely related to the mathematical objects.
-- 
-- Note, that most of the functions are not (yet) failsafe and
-- you should always make sure that the documented assumptions
-- on input arguments are satisfied in order to guarantee valid
-- results without errors.
-- 

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
    , isConnected
    , isPseudoManifold
    , isStronglyConnected
    , parentSimplices
    , star
    , eulerCharacteristic
    , dfsVertices
    , dfsSimplices
    , isValid
    , isStructurallyValid
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
                   , nub
                   , nubBy
                   , subsequences
                   , unfoldr
                   , union
 )
import Control.Monad.State
import Util ( equalLength, roundToMagnitude )

-- | A 'Vertex' is basically an 'Integer' in the default implementation.
--   (We use Integers instead of Ints because they are more
--   mathematical, but both is fine for the library.)
--   
--   If desired, vertices can contain more information, e. g. labels
--   (see the source code for an example).
--   Make sure to change 'vertexToIntegral', 'vertexFromIntegral',
--   'vertexMapCalc' and the 'Show' instance declaration accordingly.
newtype Vertex = Vertex { val :: Integer } deriving (Eq, Ord)

-- | A 'Simplex' is just a "set" of 'Vertex' elements.
type Simplex = [Vertex]

-- | A 'Complex' is a "set" of 'Simplex' elements.
type Complex = [Simplex]


-- vertices

instance Show Vertex where
    show = show . val

-- | The function 'vertexToIntegral' should define a bijection
--   between (used) vertices and some subset of the integers.
vertexToIntegral :: (Integral i) => Vertex -> i
vertexToIntegral = fromIntegral . val

-- | 'vertexFromIntegral' is more or less an inverse to 'vertexToIntegral':
-- 
--   > vertexToIntegral . vertexFromIntegral == id
--
--   should always hold, but the other composition might not be the identity
--   if vertices contain more information. (See also 'vertexMapCalc'.)
vertexFromIntegral :: (Integral i) => i -> Vertex
vertexFromIntegral = Vertex . fromIntegral

-- | To avoid the problem described at 'vertexFromIntegral',
--   we provide a function that maps a calculation over the
--   underlying integer of the 'Vertex' without losing whatever
--   context the vertex carries.
--
--   > vertexMapCalc id == id
vertexMapCalc :: (Integral i) => (i -> i) -> Vertex -> Vertex
vertexMapCalc f = vertexFromIntegral . f . vertexToIntegral

-- Alternative definition of the Vertex type and accompanying functions:
-- 
-- data Vertex = Vertex { val :: Int, label :: String } deriving (Eq, Ord)
-- instance Show Vertex where show = label
-- vertexToIntegral = fromIntegral . val
-- vertexFromIntegral k = Vertex (fromIntegral k) ("#"++(show k))
-- vertexMapCalc f (Vertex v l) = Vertex (fromIntegral . f .fromIntegral $ v) l


-- simplices

-- | Return the dimension of a simplex.
dimS :: (Integral i) => Simplex -> i
dimS s = genericLength s - 1

-- | Test whether a simplex is of specific dimension.
isNSimplex :: (Integral i) => i -> Simplex -> Bool
isNSimplex n s = dimS s == n

-- | Test whether a simplex is face of another,
--   that is, if it is a subset of the second simplex.
isFaceOf :: Simplex -> Simplex -> Bool
isFaceOf s s' = all (`elem` s') s

-- | 'vertexMapCalc' liftet to simplices.
simplexMapCalc :: (Integral i) => (i -> i) -> Simplex -> Simplex
simplexMapCalc f = map (vertexMapCalc f)


-- complexes

-- | Compute the "set" of vertices of a complex
--   (which is the union over all simplices).
vertices :: Complex -> [Vertex]
vertices = foldr union []

-- | 'simplexMapCalc' liftet to complexes.
complexMapCalc :: (Integral i) => (i -> i) -> Complex -> Complex
complexMapCalc f = map (simplexMapCalc f)

-- | Return the dimension of a complex.
dim :: (Integral i) => Complex -> i
dim = maximum . map dimS

-- | Return all facets of a complex. Here a facet is a simplex
--   that has the same dimension of the complex (which differs 
--   from the usual mathematical definition for non pure complexes).
facets :: Complex -> [Simplex]
facets c = filter (isNSimplex $ dim c) c

-- | 'generatedBy' takes a list of simplices and adds all faces
--   of those simplices. The result should always be a valid complex
--   (if the simplices were valid).
generatedBy :: [Simplex] -> Complex
generatedBy c = foldr addFaces c c
    where
        addFaces s c' = c' `union` (subsequences s)

-- | Utility function returning an integer @k@ such that
--   @'vertexToIntegral' v <= k@ for all vertices @v@ of the given complex.
calculateOffset :: (Integral i) => Complex -> i
calculateOffset =
    roundToMagnitude . (+1) . maximum . map vertexToIntegral . vertices

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

-- | Return all parent simplices of a simplex where a
--   /parent simplex of @s@/ is any simplex @s'@
--   such that @s@ is a face of @s'@.
parentSimplices :: Simplex -> [Simplex] -> [Simplex]
parentSimplices s = filter (s `isFaceOf`)

-- | Return the star of a vertex.
star :: Vertex -> Complex -> Complex
star v c = generatedBy $ parentSimplices [v] c

-- | Compute the Euler characteristic of a complex.
eulerCharacteristic :: (Integral i) => Complex -> i
eulerCharacteristic =
    foldl' f 0 . delete []
        where
            f sum s = sum + (-1)^(dimS s)

-- | Return the set of connected components of a complex.
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


-- | Perform depth first search on the 1-skeleton of a complex,
--   starting at a given vertex. Returns all found vertices.
dfsVertices :: Complex -> Vertex -> [Vertex]
dfsVertices c v = map head $ execState (dfs dfsHVertex c [v]) []

-- | Perform depth first search with simplices where two n-simplices
--   are considered adjacent if they share a common (n-1)-simplex.
--   Returns all found simplices of the same dimension as the starting
--   simplex.
dfsSimplices :: Complex -> Simplex -> [Simplex]
dfsSimplices c s = execState (dfs dfsHSimplex c s) []


-- for debug purposes

-- | Test whether a complex contains all faces of its simplices.
isValid :: Complex -> Bool
isValid c = all (containsFaces c) c
    where
        containsFaces c' s = all (`elem` c') $ subsequences s

-- | Test whether the simplices and the complex itself are proper "sets".
--   For instance, @'isValid' [[],[1],[1]] == True@, but
--   'isStructurallyValid' would return @False@.
isStructurallyValid :: Complex -> Bool
isStructurallyValid c = t1 && t2
        where 
            t1 = all (\ s -> genericLength s == (genericLength . nub) s) c
            t2 = nubBy isSameSimplex c == c
            isSameSimplex s s' = dimS s == dimS s' && s `isFaceOf` s'


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
