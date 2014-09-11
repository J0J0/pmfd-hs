
-- |
-- Module      : Surface
-- Description : Basic type for closed surfaces
-- Copyright   : (c) Johannes Prem, 2014
-- License     : ISC License

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


-- | A 'Surface' represents a closed surface.
--   It is completely determined by its genus
--   and orientability type.
data Surface = Surface {   isOrientable :: Bool
                         , genus        :: Integer
               }
               deriving (Eq, Ord)

instance Show Surface where
    show surf = non ++ "OS.g=" ++ show (genus surf)
        where
            non = if isOrientable surf then [] else "N"

-- | Convenience function that returns a more verbose
--   output than the default from the 'Show' instance.
showSurfaceVerbosly :: Surface -> String
showSurfaceVerbosly surf =
    non ++ "orientable surface of genus " ++ show (genus surf)
        where
            non = if isOrientable surf then [] else "non-"

-- | provided for convenience
isNonOrientable :: Surface -> Bool
isNonOrientable = not . isOrientable


-- | 'identifySurfaceScheme' takes a 'Scheme' and identifies
--   the closed surface it represents. Note that the input
--   has to be in normalized form, that is it must be (exactly!)
--   of the form that 'normalizeScheme' or 'canonicalizeScheme'
--   from "PolygonScheme" return.
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

-- | Convert a named surface to an actual 'Surface'.
fromNamedSurface :: NamedSurface -> Surface
fromNamedSurface surf =
    fromJust $ lookup surf namedSurfaceTable

-- | Try to convert a 'Surface' to a named surface.
--   Returns 'Nothing' if the input surface doesn't
--   have a name (of type 'NamedSurface').
toNamedSurface :: Surface -> Maybe NamedSurface
toNamedSurface surf =
    lookup surf $ map swap namedSurfaceTable
