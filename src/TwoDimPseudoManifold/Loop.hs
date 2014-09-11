
-- |
-- Module      : TwoDimPseudoManifold.Loop
-- Description : Simplify loops on 2-dim. pseudomanifolds
-- Copyright   : (c) Johannes Prem, 2014
-- License     : ISC License

module TwoDimPseudoManifold.Loop
(
      Walk
    , Loop
    , LoopS
    , isWalkIn
    , isLoopIn
    , isSimpleLoopIn
    , isTrivial
    , simplifyOnSphere
    , simplifyOnTorus
    , simplifyOnPrPlane
    , simplifyOnKleinB
) where

import Control.Arrow ( (&&&), first, second )
import Data.Function ( on )
import Data.List ( find, groupBy, nub, nubBy, partition, sortBy )
import qualified Data.Map.Strict as  M
import qualified Data.Map.Lazy   as LM
import Data.Maybe ( fromJust, mapMaybe )
import qualified Data.Traversable as T
import Data.Tuple ( swap )

import DehnAlgorithm ( dehnAlg )
import PolygonScheme ( 
                       Scheme
                     , SchemeWL(..)
                     , invSymb
                     , normalizeSchemeWL
 )
import SimplicialComplex (
                           Vertex(..)
                         , Simplex
                         , Complex
                         , dfsVertices
                         , generatedBy
                         , isFaceOf
                         , isNSimplex
                         , parentSimplices
                         , vertices
                         , vMap
 )
import Surface (
                 Surface
               , NamedSurface(..)
               , identifySurfaceScheme
               , toNamedSurface
 )
import TwoDimManifold (
                        polygonPSchemeWL
                      , preSchemeToScheme'
                      , translateVia
 )
import TwoDimPseudoManifold (
                              fixAllSingularities
 )
import TwoDimPseudoManifold.GluingGraph (
                                          GluedD(..)
                                        , GluedObj
                                        , gluingGraphFromFixed
 )
import Util ( (.:), both )


type Walk a = [Vertex a]
type Loop a = Walk a
type LoopS  = Scheme


-- | Test whether some sequence of vertices determines a
--   walk a complex.
isWalkIn :: [Vertex a] -> Complex a -> Bool
isWalkIn w c =
    all (\ (v,v') -> [v,v'] `elem` es') $ w `zip` (tail w)
        where
            es  = filter (isNSimplex 1) c
            es' = es ++ map reverse es

-- | Test whether a given walk is a loop, i. e. if the 
--   walk is closed. In this context a loop may contain
--   a vertex more than once.
isLoopIn :: Walk a -> Complex a -> Bool
isLoopIn l c =
    l `isWalkIn` c && head l == last l

-- | Test whether a loop is a simple loop, i. e. if
--   its vertices are pairwise distinct.
isSimpleLoopIn :: Loop a -> Complex a -> Bool
isSimpleLoopIn l c =
    l `isLoopIn` c && (tail l) == nub (tail l)


-- | Tests whether a loop in a pseudomanifold is contractible.
--
--   The input complex must be a 2-dim. pseudomanifold.
isTrivial :: (Eq a) => Loop a -> Complex a -> Bool
isTrivial = null . uncurry simplifyLoop . normalize .: schemesWL


-- This function computes the schemes of the closed surfaces that
-- can be glued to the input pseudomanifold (much like 'baseSurfaces'
-- from "TwoDimPseudoManifold") but also computes a representation
-- of the given loop in terms of this schemes (and other symbols
-- if necessary).
schemesWL :: (Eq a) => Loop a -> Complex a -> (GluedObj Scheme, LoopS)
schemesWL l c =
    (schs, ll')
        where
            c'     = fixAllSingularities c
            gd     = gluingGraphFromFixed c'
            
            l'     = liftLoop l c c' gd
            lpsch  = zipWith (\ x y -> [x,y]) l' (tail l')
            
            ((lpsch',t,tab), schs) =
                T.mapAccumR polySch (lpsch,1,[]) (glComplexes gd)
            polySch (ll,t,tab) comp =
                let (pSch, ll') = polygonPSchemeWL comp ll
                    (sch, tab') = preSchemeToScheme' t pSch
                    t' = fromEnum $ snd $ last tab'
                in ((ll',t'+1,tab'++tab), sch)
            
            glueC = complexFromGluedD gd
            aes   = spanningEdges glueC
            es    = mapSpanningEdges gd aes
            
            isGlV (Vertex (_,j)) = j == (-1)
            les = nubBy isFaceOf $
                    filter (\ [vv, vv'] -> isGlV vv || isGlV vv') lpsch'
            isSpanningEdge e = any (e `isFaceOf`) es
            (sp,nsp) = partition isSpanningEdge les
            tabAll = tab ++  sp `zip` repeat (toEnum 0)
                         ++ nsp `zip` [toEnum t ..]
            
            ll'  = filter (/= (toEnum 0)) $ translateVia tabAll lpsch'

-- Normalize all input schemes and additionally modify the given
-- loop to reflect the normalization steps.
normalize :: (GluedObj Scheme, LoopS) -> (GluedObj Scheme, LoopS)
normalize (schs, loop) =
    second fromJust $ swap $
        T.mapAccumR normOne (Just loop) schs
            where
                normOne l sch =
                    let swl  = SchemeWL { scheme = sch, sloop = l }
                        swl' = normalizeSchemeWL swl
                    in (sloop swl', scheme swl')

-- Once we identified the surface schemes and have a loop in terms of
-- this schemes, we use this function to reduce the loop scheme.
-- It uses Dehn's algorithm for surface schemes with more than 4 symbols
-- and the functions 'simplifyOnXX' below for the other cases.
simplifyLoop :: GluedObj Scheme -> LoopS -> LoopS
simplifyLoop schs l =
    simplifyLoop' xs os l'
        where
            isSurf surf sch = Just surf ==
                (toNamedSurface . identifySurfaceScheme $ sch)
                
            (ss,nss) = partition (isSurf Sphere) (LM.elems schs)
            l' = foldr simplifyOnSphere l ss
            
            (xs, os) = foldr f ([],nss) [Torus, ProjectivePlane, KleinBottle]
            f surf (xs,os') =
                first (:xs) $ partition (isSurf surf) os'

simplifyLoop' :: [[Scheme]] -> [Scheme] -> LoopS -> LoopS
simplifyLoop' _ _ [] = []
simplifyLoop' xs@[tori,planes,bottles] others l =
    let 
         l'  = foldr (\ (f,x) ll -> foldr f ll x) l $ zip 
                 [simplifyOnTorus, simplifyOnPrPlane, simplifyOnKleinB] xs
         
         l'' = dehnAlg others l'
    in
        if l /= l'' then simplifyLoop' xs others l'' else l


-- | Reduces the parts of a loop that live on a 2-sphere.
--   Since the 2-sphere is simply connected, every loop is
--   trivial here.
simplifyOnSphere :: Scheme -> LoopS -> LoopS
simplifyOnSphere sch l =
    l >>= (\ x -> if x `elem` sch then [] else [x])

-- | Reduces the parts of a loop that live on the torus.
simplifyOnTorus :: Scheme -> LoopS -> LoopS
simplifyOnTorus sch l =
    concat $ zipWith ($) fff chunks
        where
            chunks = groupBy ((==) `on` (`elem` sch)) l
            fff    = cycle $ case chunks of
                               (z:_):_ | z `elem` sch -> [f,id] 
                               _                      -> [id,f]
            f = sortBy (compare `on` (abs . fromEnum)) 

-- | Reduces the parts of a loop that live on a projective plane.
simplifyOnPrPlane :: Scheme -> LoopS -> LoopS
simplifyOnPrPlane [x,y,_,_] l =
    concat $ zipWith ($) fff chunks
        where
            l' = l >>= (\ z -> if z == y then [] else [z])
            chunks = groupBy ((==) `on` (`elem` [x,invSymb x])) l'
            fff    = cycle $ case chunks of
                               (z:_):_ | z `elem` [x,invSymb x] -> [f,id] 
                               _                                -> [id,f]
            f ch = if even (length ch)
                   then [] else [x] 

-- | Reduces the parts of a loop that live on a Klein bottle.
simplifyOnKleinB :: Scheme -> LoopS -> LoopS
simplifyOnKleinB sch@[x,_,_,y] l =
    concat $ zipWith ($) fff chunks
        where
            chunks = groupBy ((==) `on` (`elem` [x,ix,y,iy])) l
            fff    = cycle $ case chunks of
                               (z:_):_ | z `elem` [x,ix,y,iy] -> [f,id] 
                               _                              -> [id,f]
            
            add (p,q) (p',q') = (p+p', q+q')
            f ch = case foldr (\ (g,z) -> add (g z))
                              (0,0) (ggg `zip` ch)
                   of (0,0) -> []
                      _     -> ch
            
            ix  = invSymb x
            iy  = invSymb y
            ggg = cycle [g1,g2]
            
            g1 z | z == x   =  (0, 1)
                 | z == y   =  (0,-1)
                 | z == ix  =  (-1,0)
                 | z == iy  =  ( 1,0)
                 
            g2 z | z == x   =  ( 1,0)
                 | z == y   =  (-1,0)
                 | z == ix  =  (0, 1)
                 | z == iy  =  (0,-1)


-- This function lifts a loop from a pseudomanifold @c@ to the
-- complex @c'@ returned by 'fixAllSingularities'. 
-- In the case where the input loop passes any singularities of
-- @c@, the returned loop, however, cannot actually be a loop
-- in @c'@ because the vertex at the singularity has been split up.
-- In such cases 'liftV' (see below) introduces a pseudo vertex with
-- index -1 in order to preserve the information in our loop.
liftLoop :: (Eq a) =>
             Loop a -> Complex a -> Complex (a, Int) ->
                 GluedD a -> Loop (a, Int)
liftLoop l c c' gd =
    [last l'] ++ l'
        where
            glvs = LM.elems (glVertices gd)
            vs = zip3 l (tail l) (drop 2 $ l ++ [l !! 1])
            liftV' cc glvs (fv,cv,tv)
                | cv `notElem` glvs  =  [vMap (id &&& const 0) cv]
                | otherwise          =  liftV cc fv cv tv
            l' = concatMap (liftV' c' glvs) vs

    
liftV :: (Eq a) => 
            Complex (a, Int) -> Vertex a -> Vertex a -> Vertex a ->
                [Vertex (a, Int)]
liftV c' fv cv tv =
    let cvs' = filter (\ v -> vMap fst v == cv) $ vertices c'
        vss  = map ((\ (Vertex (_,j)) -> j) &&& adjVs c') cvs'
        adjVs cc v = map (vMap fst) $ vertices $ parentSimplices [v] cc
        av v = head $ mapMaybe (\ (j,vs) -> if v `elem` vs
                                             then Just j else Nothing) vss
        (jfv, jtv) = both av (fv, tv)
    in
        if jfv == jtv
            then [vMap (id &&& const jfv) cv]
            else map (\ j -> vMap (id &&& const j) cv) [jfv, -1, jtv] 
            


complexFromGluedD :: GluedD a -> Complex (Either Int Int)
complexFromGluedD gd =
    generatedBy $ map (map Vertex) $
        map (\ (x,y) -> [Left y, Right x]) $ M.keys (glGraphD gd)


mapSpanningEdges :: (Eq a) =>
                        GluedD a -> [Simplex (Either Int Int)] ->
                            [Simplex (a, Int)]
mapSpanningEdges gd es =
    map replaceEdge es
        where
            replaceEdge [Vertex (Left j), Vertex (Right j')] =
                let Just comp = LM.lookup j  (glComplexes gd)
                    Just v    = LM.lookup j' (glVertices gd)
                    v' = head $ filter (\ vv -> vMap fst vv == v)
                                       (vertices comp)
                in [v', vMap (id &&& const (-1)) v]
    


spanningEdges :: Complex a -> [Simplex a]
spanningEdges c =
    fst $ foldr (addEdge ss) ([],[v]) (init vs)
        where
            v  = head $ vertices c
            vs = dfsVertices c v
            ss = filter (isNSimplex 1) c
            addEdge ss' v' (es,vs') =
                let (e:_) = mapMaybe
                                (\ vv -> find ([v',vv] `isFaceOf`) ss') vs'
                in (e:es, v':vs')
