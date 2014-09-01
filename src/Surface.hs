
module Surface
(
      Surface(..)
    , isNonOrientable
    , showSurfaceVerbosly
    , identifySurfaceScheme
    , NamedSurface(..)
    , fromNamedSurface
    , toNamedSurface
    
) where

import Data.List ( genericLength )
import Data.Maybe ( fromJust )
import Data.Tuple ( swap )

import PolygonScheme ( Scheme, invSymb )


data Surface = Surface {   isOrientable :: Bool
                         , genus        :: Integer
               }
               deriving (Eq, Ord)

instance Show Surface where
    show surf = non ++ "OS.g=" ++ show (genus surf)
        where
            non = if isOrientable surf then [] else "N"

showSurfaceVerbosly :: Surface -> String
showSurfaceVerbosly surf =
    non ++ "orientable surface of genus " ++ show (genus surf)
        where
            non = if isOrientable surf then [] else "non-"

isNonOrientable :: Surface -> Bool
isNonOrientable = not . isOrientable

identifySurfaceScheme :: Scheme -> Surface
identifySurfaceScheme sch@[x,xx,y,yy]
    | x == invSymb xx     =  fromNamedSurface Sphere
    | x == y && xx == yy  =  fromNamedSurface ProjectivePlane
    | x == invSymb y      =  fromNamedSurface Torus
    | x == y && xx /= yy  =  fromNamedSurface KleinBottle
identifySurfaceScheme sch@(x:xx:_)
    | x /= xx  =  Surface True  (len `div` 4)
    | x == xx  =  Surface False (len `div` 2)
        where
            len = genericLength sch

data NamedSurface = Sphere | Torus | ProjectivePlane | KleinBottle
                    deriving (Eq, Show)

namedSurfaceTable :: [(NamedSurface, Surface)]
namedSurfaceTable =
    let (-) = (,) in [
      Sphere           -  Surface True  0
    , Torus            -  Surface True  1
    , ProjectivePlane  -  Surface False 1
    , KleinBottle      -  Surface False 2
    ]

fromNamedSurface :: NamedSurface -> Surface
fromNamedSurface surf =
    fromJust $ lookup surf namedSurfaceTable

toNamedSurface :: Surface -> Maybe NamedSurface
toNamedSurface surf =
    lookup surf $ map swap namedSurfaceTable
