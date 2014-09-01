
module PolygonScheme
(
      Symbol
    , Scheme
    , invSymb
    , invSch
    , isProperScheme
    , normalizeScheme
    , renameScheme
    , canonicalizeScheme
) where

import Control.Arrow ( first, second )
import Control.Monad.State
import Data.Char ( chr )
import qualified Data.Foldable as F ( all )
import Data.List ( foldr, reverse )
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


splitAfterPairs :: Scheme -> (Scheme, Scheme)
splitAfterPairs [] = ([],[])
splitAfterPairs xs@(x:xx:xs')
    | x == xx    = let (ps,rs) = splitAfterPairs xs'
                   in (x:x:ps,rs)
    | otherwise  = ([],xs)


-- move aa in front
step1 :: Scheme -> Scheme
step1 = step1h [] [] 

step1h :: Scheme -> Scheme -> Scheme -> Scheme
step1h pairs pre [] = pairs ++ reverse pre
step1h pairs pre (x:xs) =
    case break (==x) xs of
        (_,[])      -> step1h pairs (x:pre) xs
        (ys,(_:zs)) -> step1h (x:x:pairs) [] (reverse pre ++ invSch ys ++ zs)


-- remove aa'
step2 :: Scheme -> Scheme
step2 = step2h True []

step2h :: Bool -> Scheme -> Scheme -> Scheme
step2h check s1 s2
    | len2 < 2 || (check && len1 + len2 <= 4)  = reverse s1 ++ s2
        where
            len1 = length s1
            len2 = length s2
step2h check s1 (y:yy:ys)
    | y == invSymb yy   = case s1 of
                            []      -> step2h check [] ys
                            (x:xs)  -> step2h check xs (x:ys)
    | otherwise         = step2h check (y:s1) (yy:ys)
    
    
-- group aba'b' together
step3 :: Scheme -> Scheme
step3 s = pairs ++ step3h [] rest
    where
        (pairs,rest) = splitAfterPairs s

step3h :: Scheme -> Scheme -> Scheme
step3h pre xs
    | length xs <= 4  = reverse pre ++ xs
step3h pre (x:xs) =
    let (ys,_:zs) = break (== (invSymb x)) xs in
    case step3hh [] ys zs of
        Nothing         -> step3h (x:pre) xs
        Just (y, rest)  -> let ix = invSymb x
                               iy = invSymb y
                           in [x,y,ix,iy] ++ step3h [] (step2h False [] rest)
    where
        step3hh :: Scheme -> Scheme -> Scheme -> Maybe (Symbol,Scheme)
        step3hh _ [] _ = Nothing
        step3hh pre' (y:ys) zs =
            case break (== (invSymb y)) zs of
                (_,[])        -> step3hh (y:pre') ys zs
                (zs',_:zs'')  -> Just (y, nw)
                    where
                        nw = r pre ++ zs' ++ ys ++ r pre' ++ zs''
                        r = reverse


-- convert ccaba'b' to aabbcc
step4 :: Scheme -> Scheme
step4 xs
    | length xs <= 4  = xs
step4 xs@(x:xx:_)
    | x /= xx    = xs
    | otherwise  = step4h xs
    
step4h :: Scheme -> Scheme
step4h xs@(_:_:[]) = xs
step4h (x:_:xs@(xx:xxx:_))
    | xx == xxx  = x:x:step4h xs
    | otherwise  = step4hh xs ++ [x,x]

step4hh :: Scheme -> Scheme
step4hh [] = []
step4hh (y:z:_:_:rest') = y:y:z:z:step4hh rest'


-- handle schemes with four symbols
step5 :: Scheme -> Scheme
step5 xs@[x,xx,y,yy]
    | x == xx         &&  y == yy          =  [x,invSymb y,x,y]
    | x == xx         &&  y == invSymb yy  =  [x,y,x,y]
    | x == invSymb yy && xx == invSymb y   =  [yy,x,xx,y]
step5 xs = xs

-- do all steps
normalizeScheme :: Scheme -> Scheme
normalizeScheme = step5 . step4 . step3 . step2 . step1

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
