
-- |
-- Module      : SimplicialComplex.Generator
-- Description : Complex building and modifying functions
-- Copyright   : (c) Johannes Prem, 2014
-- License     : ISC License

module SimplicialComplex.Generator where

import Control.Arrow ( (&&&) )
import Data.List (
                     (\\)
                   , concatMap
                   , nub
                   , foldl
                   , foldr1
                   , sort
                   , union
 )
import Data.Maybe ( fromMaybe )

import SimplicialComplex
import Util ( (.:), both, filter2 )


type GlueData a = [(Vertex a,Vertex a)]

glueVertices :: GlueData a -> Complex a -> Complex a
glueVertices glueData c =
    nub $ map (nub . map projectVertex) c
        where
            projectVertex v = fromMaybe v $ lookup v glueData

triangulatedRectangle :: Int -> Int -> Complex Int
triangulatedRectangle nx ny = triangulatedRectangle' nx ny []

triangulatedRectangle' :: Int -> Int -> [(Int,Int)] -> Complex Int
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

lexiV :: Int -> (Int, Int) -> Vertex Int
lexiV nx (x,y) = Vertex $ x + (nx+1)*(y-1)

data GlueOr   = SameOr | OppOr  deriving (Eq, Ord, Enum, Show)
data GlueSpec = GlueSpec { xOr :: GlueOr, yOr :: GlueOr }  deriving (Eq, Show)

gluedRectangle :: GlueSpec -> Complex Int
gluedRectangle = gluedRectangle' 3 3

gluedRectangle' :: Int -> Int -> GlueSpec -> Complex Int
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

fromIntData :: [(Int,Int)] -> GlueData Int
fromIntData = map (both Vertex)


disjointUnion :: (Eq a, Eq b) => 
                    Complex a -> Complex b -> Complex (Either a b)
disjointUnion c c' =
   complexMap Left c `union` complexMap Right c'

disjointUnions :: (Eq a) => [Complex a] -> Complex (a, Int)
disjointUnions = disjointUnionsInd 1

disjointUnionsInd :: (Eq a) => 
                        Int -> [Complex a] -> Complex (a, Int)
disjointUnionsInd t cs =
    concatMap f $ cs `zip` [t..]
        where
            f (c,j) = complexMap (id &&& const j) c
            -- > id &&& const j  ==  flip (,) j
            -- but i find the first variant more comprehensible

connectedSumAt :: (Eq a, Eq b) =>
                    Simplex a -> Complex a -> Simplex b -> Complex b ->
                        Complex (Either a b)
connectedSumAt s1 c1 s2 c2 =
    let cc  = disjointUnion c1 c2
        s1' = simplexMap Left s1
        s2' = simplexMap Right s2
    in innerConnectedSumAt s1' s2' cc

innerConnectedSumAt :: (Eq a) =>
                        Simplex a -> Simplex a -> Complex a -> Complex a
innerConnectedSumAt s1 s2 c =
    let gd  = s2 `zip` s1
        c' = if isNSimplex 0 s1 then c else c \\ [s1,s2]
    in glueVertices gd c'

innerConnectedSum :: (Integral i, Eq a) =>
                        i -> (a -> Bool) -> (a -> Bool) ->
                            Complex a -> Complex a
innerConnectedSum d p q c =
    let pred p' s    = isNSimplex d s && let (Vertex x:_) = s in p' x
        (s1:_, s2:_) = filter2 (pred p) (pred q) c
    in innerConnectedSumAt s1 s2 c

connectedSum, (#) :: (Eq a, Eq b) =>
                    Complex a -> Complex b -> Complex (Either a b)
connectedSum c1 c2 =
    let d = min (dim c1) (dim c2)
    in connectedSum' d c1 c2

infixr 9 #
(#) = connectedSum

connectedSum' :: (Integral i, Eq a, Eq b) =>
                    i -> Complex a -> Complex b -> Complex (Either a b)
connectedSum' d c1 c2 =
    connectedSumAt s1 c1 s2 c2
        where
            s1 = findDSimplex c1
            s2 = findDSimplex c2
            findDSimplex = head . filter (isNSimplex d) 

connectedSums :: (Eq a) => [Complex a] -> Complex (a, Int)
connectedSums cs =
    let d = minimum $ map dim cs
    in connectedSums' d cs

connectedSums' :: (Integral i, Eq a) =>
                    i -> [Complex a] -> Complex (a, Int)
connectedSums' d cs =
    foldr connect ucs [2 .. length cs]
        where
            ucs        = disjointUnionsInd 1 cs
            connect j  = innerConnectedSum d (matchInd (< j)) (matchInd (==j))
            matchInd p (_,t) = p t


wedgeSumAt :: (Eq a, Eq b) =>
                Vertex a -> Complex a -> Vertex b -> Complex b ->
                     Complex (Either a b)
wedgeSumAt v1 c1 v2 c2 =
    connectedSumAt [v1] c1 [v2] c2

wedgeSum, (\/) :: (Eq a, Eq b) => Complex a -> Complex b -> Complex (Either a b)
wedgeSum = connectedSum' 0

infixr 9 \/
(\/) = wedgeSum

wedgeSums :: (Eq a) => [Complex a] -> Complex (a, Int)
wedgeSums cs@(c:cs') =
    let ucs = disjointUnionsInd 1 cs
        newBaseP = vMap (id &&& const 1) $ head $ vertices c
        bps = [ Vertex (v,j) | (c',j) <- cs' `zip` [2..],
                               let Vertex v = head $ vertices c' ]
        gd  = bps `zip` repeat newBaseP
    in glueVertices gd ucs
