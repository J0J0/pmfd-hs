{-# LANGUAGE GADTs, GADTSyntax, StandaloneDeriving #-}

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
      Vertex(..)
    , Simplex
    , Complex
    , vMap
    , simplexMap
    , dimS
    , isNSimplex
    , isFaceOf
    , vertices
    , complexMap
    , dim
    , generatedBy
    , facets
    , connectedComponents
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

-- | A 'Vertex' can be anything you like that is an intance of 'Eq'.
data Vertex a where
    Vertex :: (Eq a) => a -> Vertex a

deriving instance Eq (Vertex a)
deriving instance (Ord a) => Ord (Vertex a)
instance (Show a) => Show (Vertex a) where show (Vertex v) = show v

-- | A 'Simplex' is just a "set" of 'Vertex' elements.
type Simplex a = [Vertex a]

-- | A 'Complex' is a "set" of 'Simplex' elements.
type Complex a = [Simplex a]


-- vertices

-- can't make a Functor instance because b needs to be an Eq instance :(
vMap :: (Eq a, Eq b) => (a -> b) -> Vertex a -> Vertex b
vMap f (Vertex v) = Vertex (f v)


-- simplices

simplexMap :: (Eq a, Eq b) => (a -> b) -> Simplex a -> Simplex b
simplexMap f = map $ vMap f

-- | Return the dimension of a simplex.
dimS :: (Integral i) => Simplex a -> i
dimS s = genericLength s - 1

-- | Test whether a simplex is of specific dimension.
isNSimplex :: (Integral i) => i -> Simplex a -> Bool
isNSimplex n s = dimS s == n

-- | Test whether a simplex is face of another,
--   that is, if it is a subset of the second simplex.
isFaceOf :: Simplex a -> Simplex a -> Bool
isFaceOf s s' = all (`elem` s') s


-- complexes

-- | Compute the "set" of vertices of a complex
--   (which is the union over all simplices).
vertices :: Complex a -> [Vertex a]
vertices = foldr union []

complexMap :: (Eq a, Eq b) => (a -> b) -> Complex a -> Complex b
complexMap f =
    map $ map $ vMap f

-- | Return the dimension of a complex.
dim :: (Integral i) => Complex a -> i
dim = maximum . map dimS

-- | Return all facets of a complex. Here a facet is a simplex
--   that has the same dimension of the complex (which differs 
--   from the usual mathematical definition for non pure complexes).
facets :: Complex a -> [Simplex a]
facets c = filter (isNSimplex $ dim c) c

-- | 'generatedBy' takes a list of simplices and adds all faces
--   of those simplices. The result should always be a valid complex
--   (if the simplices were valid).
generatedBy :: [Simplex a] -> Complex a
generatedBy c = foldr addFaces c c
    where
        addFaces s c' = c' `union` (subsequences s)


isConnected :: Complex a -> Bool
isConnected c = equalLength allVertices foundVertices
    where
        allVertices = vertices c
        foundVertices = dfsVertices c $ head allVertices

isPseudoManifold :: Complex a -> Bool
isPseudoManifold c = isNPseudoManifold (dim c) c

isNPseudoManifold :: (Integral i) => i -> Complex a -> Bool
isNPseudoManifold n c = all (validSimplex n c) c
    where
        validSimplex n c' s =
            let parentFacets = filter (isNSimplex n) $ parentSimplices s c'
            in case compare (dimS s) (n-1) of
                GT -> True
                EQ -> length parentFacets == 2
                LT -> not . null $ parentFacets

isStronglyConnected :: Complex a -> Bool
isStronglyConnected c = isNStronglyConnected (dim c) c

isNStronglyConnected :: (Integral i) => i -> Complex a -> Bool
isNStronglyConnected n c = equalLength nSimplices foundSimplices
    where
        nSimplices = filter (isNSimplex n) c
        foundSimplices = dfsSimplices c $ head nSimplices

-- | Return all parent simplices of a simplex where a
--   /parent simplex of @s@/ is any simplex @s'@
--   such that @s@ is a face of @s'@.
parentSimplices :: Simplex a -> [Simplex a] -> [Simplex a]
parentSimplices s = filter (s `isFaceOf`)

-- | Return the star of a vertex.
star :: Vertex a -> Complex a -> Complex a
star v c = generatedBy $ parentSimplices [v] c

-- | Compute the Euler characteristic of a complex.
eulerCharacteristic :: (Integral i) => Complex a -> i
eulerCharacteristic =
    foldl' f 0 . delete []
        where
            f sum s = sum + (-1)^(dimS s)

-- | Return the set of connected components of a complex.
connectedComponents :: Complex a -> [Complex a]
connectedComponents c = unfoldr (findConnectedComponent c) $ vertices c

findConnectedComponent :: Complex a -> [Vertex a] -> Maybe (Complex a, [Vertex a])
findConnectedComponent _ [] = Nothing
findConnectedComponent c vs =
    Just (component, remainingVertices)
        where
            vsComp = dfsVertices c $ head vs
            component = [] : filter (not . null . intersect vsComp) c
            remainingVertices = vs \\ vsComp


-- | Perform depth first search on the 1-skeleton of a complex,
--   starting at a given vertex. Returns all found vertices.
dfsVertices :: Complex a -> Vertex a -> [Vertex a]
dfsVertices c v = map head $ execState (dfs dfsHVertex c [v]) []

-- | Perform depth first search with simplices where two n-simplices
--   are considered adjacent if they share a common (n-1)-simplex.
--   Returns all found simplices of the same dimension as the starting
--   simplex.
dfsSimplices :: Complex a -> Simplex a -> [Simplex a]
dfsSimplices c s = execState (dfs dfsHSimplex c s) []


-- for debug purposes

-- | Test whether a complex contains all faces of its simplices.
isValid :: Complex a -> Bool
isValid c = all (containsFaces c) c
    where
        containsFaces c' s = all (`elem` c') $ subsequences s

-- | Test whether the simplices and the complex itself are proper "sets".
--   For instance, @'isValid' [[],[1],[1]] == True@, but
--   'isStructurallyValid' would return @False@.
isStructurallyValid :: Complex a -> Bool
isStructurallyValid c = t1 && t2
        where 
            t1 = all (\ s -> genericLength s == (genericLength . nub) s) c
            t2 = nubBy isSameSimplex c == c
            isSameSimplex s s' = dimS s == dimS s' && s `isFaceOf` s'


-- private

mark :: Simplex a -> State [Simplex a] ()
mark s = modify (s:)
marked :: Simplex a -> State [Simplex a] Bool
marked s = gets (s `elem`)
dfs :: DfsHFunc a -> Complex a -> Simplex a -> State [Simplex a] ()
dfs adjObj c s = do
    visited <- marked s
    if visited
    then return ()
    else do
        mark s
        mapM_ (dfs adjObj c) $ adjObj s c

type DfsHFunc a = Simplex a -> Complex a -> [Simplex a]

dfsHVertex :: DfsHFunc a
dfsHVertex s@[v] c =
    let edges = filter (isNSimplex 1) $ parentSimplices s c
    in map (delete v) edges

dfsHSimplex :: DfsHFunc a
dfsHSimplex s c =
    let n = dimS s
        isAdjacend = isNSimplex (n-1) . (s `intersect`)
    in filter (\ s' -> isNSimplex n s' && isAdjacend s') c
