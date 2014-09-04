
module PolygonScheme
(
      Symbol
    , Scheme
    , SchemeWL(..)
    , toSchemeWL
    , invSymb
    , invSch
    , isProperScheme
    , reduce
    , normalizeScheme
    , normalizeSchemeWL
    , renameScheme
    , canonicalizeScheme
) where

import Control.Arrow ( first, second )
import Control.Monad.State
import Data.Char ( chr )
import qualified Data.Foldable as F ( all )
import Data.List ( foldr, reverse )
import Data.Maybe ( fromMaybe )
import qualified Data.Map.Strict as M

newtype Symbol = Symbol Integer deriving (Eq, Ord)
type Scheme = [Symbol]


instance Show Symbol where
    show (Symbol k)
        | absk <= 26  = char : maybeInv
        | otherwise   = show k
            where
                absk = abs $ fromIntegral k
                char = chr $ absk + 96   -- 97 is ascii 'a'
                maybeInv = if k < 0
                           then "'"
                           else []

instance Enum Symbol where
    toEnum k = Symbol $ fromIntegral k
    fromEnum (Symbol k) = fromIntegral k


invSymb :: Symbol -> Symbol
invSymb (Symbol k) = Symbol (-k)

invSch :: Scheme -> Scheme
invSch = reverse . map invSymb


isProperScheme :: Scheme -> Bool
isProperScheme =
    F.all (==2) .
        foldr (\ (Symbol k) -> M.insertWith (+) (abs k) 1) M.empty


splitAfterLastPair :: Scheme -> (Scheme, Scheme)
splitAfterLastPair xs@(x:xx:xs')
    | x == xx    =
        let (ps,rs) = splitAfterLastPair xs'
        in (x:x:ps,rs)
splitAfterLastPair xs = ([],xs)

splitBeforeLastPair :: Scheme -> (Scheme, Scheme)
splitBeforeLastPair (x:xx:xs'@(y:yy:_))
    | x == xx && y == yy  =
        let (ps, rs) = splitBeforeLastPair xs'
        in (x:xx:ps, rs)         
splitBeforeLastPair xs = ([], xs)


-- normalizing a scheme

data SchemeWL  = SchemeWL {   scheme :: Scheme
                            , sloop  :: Maybe Scheme }
                 deriving (Show)

loop :: SchemeWL -> Scheme
loop = fromMaybe [] . sloop

toSchemeWL :: Scheme -> SchemeWL
toSchemeWL sch = SchemeWL sch Nothing

type SchemeWLst = State Scheme Scheme

-- modify loop

reduce :: Scheme -> Scheme
reduce sch = evalState (step2h False [] sch) []  -- see below for step2h

subs1 :: Symbol -> Scheme -> Scheme -> Scheme
subs1 s sch =
    subs' (\ x -> case () of
                    _ | x == s    ->  sch
                      | x == is   -> isch
                      | otherwise ->  [x] )
        where
            is = invSymb s
            isch = invSch sch

subs2 :: Symbol -> Scheme -> Symbol -> Scheme -> Scheme -> Scheme
subs2 s1 sch1 s2 sch2 =
    subs' (\ x -> case () of
                    _ | x == s1   ->  sch1
                      | x == is1  -> isch1
                      | x == s2   ->  sch2
                      | x == is2  -> isch2
                      | otherwise ->   [x] )
        where
            is1 = invSymb s1
            is2 = invSymb s2
            isch1 = invSch sch1
            isch2 = invSch sch2

subs' :: (Symbol -> Scheme) -> Scheme -> Scheme
subs' f sch = sch >>= f

-- statefull versions of 'subs1' and 'subs2' that also reduce after each subs
subs1rS :: Symbol -> Scheme -> State Scheme ()
subs1rS s sch = modify (reduce . subs1 s sch)

subs2rS :: Symbol -> Scheme -> Symbol -> Scheme -> State Scheme ()
subs2rS s1 sch1 s2 sch2 = modify (reduce . subs2 s1 sch1 s2 sch2)

-- move aa in front
step1 :: Scheme -> SchemeWLst
step1 = step1h [] [] 

step1h :: Scheme -> Scheme -> Scheme -> SchemeWLst
step1h pairs pre [] = return $ pairs ++ reverse pre
step1h pairs pre (x:xs) =
    case break (==x) xs of
        (_,[])      -> step1h pairs (x:pre) xs
        (ys,(_:zs)) -> do
                        unless (null ys && (null pre || null zs)) $
                               subs1rS x (invSch $ zs ++ [x] ++ pre)
                        step1h (x:x:pairs) [] (reverse pre ++ invSch ys ++ zs)


-- remove aa'
step2 :: Scheme -> SchemeWLst
step2 = step2h True []

step2h :: Bool -> Scheme -> Scheme -> SchemeWLst
step2h check s1 s2
    | len2 < 2 || (check && len1 + len2 <= 4) =
        return $ reverse s1 ++ s2
        where
            len1 = length s1
            len2 = length s2
step2h check s1 (y:yy:ys)
    | y == invSymb yy   = do subs1rS y []
                             case s1 of
                                []      -> step2h check [] ys
                                (x:xs)  -> step2h check xs (x:ys)
    | otherwise         = step2h check (y:s1) (yy:ys)
    
    
-- group aba'b' together
step3 :: Scheme -> SchemeWLst
step3 s = do
        rest' <- step3h [] rest
        return $ pairs ++ rest'
            where
                (pairs,rest) = splitAfterLastPair s

step3h :: Scheme -> Scheme -> SchemeWLst
step3h y1r xs
    | length xs <= 4  = return $ reverse y1r ++ xs
step3h y1r (x:xs) = do
    let (ys,_:zs) = break (== (invSymb x)) xs
    case step3hh [] ys zs of
        Nothing              -> step3h (x:y1r) xs
        Just (y,y2,y3,y4,y5) -> do
            let ix = invSymb x
                iy = invSymb y
                y1 = reverse y1r
                rest = y1 ++ y4 ++ y3 ++ y2 ++ y5
            subs1rS x (invSch $ y2 ++ [y] ++ y3 ++ [ix] ++ y1)
            subs2rS y (invSch $ y3 ++ [y,ix,iy] ++ y2)
                    x [iy]
            subs2rS y (invSch $ [y,ix,iy] ++ y1 ++ y4 ++ y3)
                    x [iy]
            rest' <- (return rest >>= step2h False [] >>= step3h [])
            return $ [x,y,ix,iy] ++ rest'
    where
        step3hh :: Scheme -> Scheme -> Scheme -> 
                        Maybe (Symbol,Scheme,Scheme,Scheme,Scheme)
        step3hh _ [] _ = Nothing
        step3hh pre (y':ys') zs =
            case break (== (invSymb y')) zs of
                (_,[])        -> step3hh (y':pre) ys' zs
                (zs',_:zs'')  -> Just (y', reverse pre, ys', zs', zs'')


-- convert ccaba'b' to aabbcc
step4 :: Scheme -> SchemeWLst
step4 xs
    | length xs <= 4  = return xs
step4 xs@(x:xx:_)
    | x /= xx    = return xs
    | otherwise  = do let (pairs, (x:_:rest)) = splitBeforeLastPair xs
                      step4h pairs x rest

step4h :: Scheme -> Symbol -> Scheme -> SchemeWLst
step4h w0 x [] = return $ w0 ++ [x,x]
step4h w0 x (y:z:_:_:rest') = do
    let w1w0 = rest' ++ w0
    subs1rS x (invSch $ [y,z,x] ++ w1w0)
    subs1rS z (invSch $ [y,x] ++ w1w0 ++ [z,y])
    subs1rS y (invSch $ [x] ++ w1w0 ++ [y,z,z])
    step4h (w0 ++ [y,y,z,z]) x rest'


-- handle schemes with four symbols
step5' :: Scheme -> Scheme
step5' xs@[x,xx,y,yy]
    | x == xx         &&  y == yy          =  [x,invSymb y,x,y]
    | x == xx         &&  y == invSymb yy  =  [x,y,x,y]
    | x == invSymb yy && xx == invSymb y   =  [yy,x,xx,y]
step5' xs = xs

step5 :: Scheme -> SchemeWLst
step5 = return . step5'

-- do all steps
normalizeSchemeWL :: SchemeWL -> SchemeWL
normalizeSchemeWL sch =
    SchemeWL sch' (sloop sch >> Just loop')
    where
        (sch', loop') = runState (allSteps $ scheme sch) (loop sch)
        allSteps sch  = return sch >>= step1 >>= step2
                       >>= step3 >>= step4 >>= step5

normalizeScheme :: Scheme -> Scheme
normalizeScheme =
    scheme . normalizeSchemeWL . toSchemeWL


-- rename symbols to obtain obtain canonical form

renameScheme :: Scheme -> (Scheme, Symbol -> Symbol)
-- the only exception is the Klein bottle: 
renameScheme xs@[x,xx,y,yy]
    | x == y && xx == invSymb yy  =  (map rf xs, rf)
        where
            ix = invSymb x
            rf z | z == x     =  toEnum 1
                 | z == xx    =  toEnum (-2)
                 | z == ix    =  toEnum (-1)
                 | z == yy    =  toEnum 2
                 | otherwise  =  z
renameScheme xs =
    let (xs', (m, _)) = runState (renameSchemeH xs) (M.empty, toEnum 1)
    in (xs', renameFunctionFromMap m)

type RenameState = (M.Map Integer Symbol, Symbol)

rnLookup :: Integer -> State RenameState (Maybe Symbol)
rnLookup k = gets (M.lookup k . fst)
rnBecomes :: Integer -> Symbol -> State RenameState ()
k `rnBecomes` x = modify $ first $ M.insert k x
rnNextSymb :: State RenameState Symbol
rnNextSymb = do
    (m, x) <- get
    put (m, succ x)
    return x

rnSgn :: Integer -> Symbol -> Symbol
rnSgn k x = if k < 0 then invSymb x else x

renameSchemeH :: Scheme -> State RenameState Scheme
renameSchemeH [] = return []
renameSchemeH (x@(Symbol k):xs) = do
    let ak = abs k
    jy <- rnLookup ak
    case jy of
        (Just y) -> do rest <- renameSchemeH xs
                       return (rnSgn k y:rest)
        Nothing  -> do y <- rnNextSymb
                       ak `rnBecomes` (rnSgn k y)
                       rest <- renameSchemeH xs
                       return (y:rest)

renameFunctionFromMap :: (M.Map Integer Symbol) -> Symbol -> Symbol
renameFunctionFromMap m x@(Symbol k) =
    case M.lookup (abs k) m of
        Nothing  -> x
        (Just y) -> rnSgn k y

canonicalizeScheme :: Scheme -> Scheme
canonicalizeScheme = fst . renameScheme . normalizeScheme
