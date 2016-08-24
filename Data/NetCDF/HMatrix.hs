{-# LANGUAGE FlexibleInstances, ConstraintKinds, TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans -fwarn-missing-methods #-}
-- | NetCDF store instance for HMatrix vectors and matrices.
module Data.NetCDF.HMatrix
       ( HVector (..)
       , HMatrix (..)
       ) where

import Data.NetCDF.Store

import qualified Numeric.LinearAlgebra as C
import Numeric.LinearAlgebra.Devel (unsafeFromForeignPtr, unsafeToForeignPtr, matrixFromVector, MatrixOrder(..))
import Foreign.C

newtype HVector a = HVector (C.Vector a)
                  deriving (Eq, Show)

instance NcStore HVector where
  type NcStoreExtraCon HVector a = (C.Element a, C.Container C.Vector a)
  toForeignPtr (HVector v) = let (a,o,n) = unsafeToForeignPtr v
                              in if o /= 0 then error "toForeignPtr: nonzero offset"
                                           else (a,n)
  fromForeignPtr p s = HVector $ unsafeFromForeignPtr p 0 (Prelude.product s)
  smap f (HVector v) = HVector $ C.cmap f v

data HMatrix a = HMatrix (C.Matrix a)

instance NcStore HMatrix where
  type NcStoreExtraCon HMatrix a = (Num a, C.Element a, C.Container C.Vector a, C.Container C.Matrix a)
  toForeignPtr (HMatrix m) = let (a,o,n) = unsafeToForeignPtr $ C.flatten m
                              in if o /= 0 then error "toForeignPtr: nonzero offset"
                                           else (a,n)
  fromForeignPtr p s =
    let c = last s
        d = product s
    in HMatrix $ matrixFromVector RowMajor (d `div` c) (last s) $
       unsafeFromForeignPtr p 0 (Prelude.product s)
  smap f (HMatrix m) = HMatrix $ C.cmap f m

-- instance C.Element CShort
-- -- instance C.Element CInt
-- instance C.Element CFloat
-- instance C.Element CDouble

-- instance C.Container C.Vector CFloat
-- instance C.Container C.Vector CDouble
-- instance C.Container C.Vector CShort

-- instance C.Indexable (C.Vector CFloat) CFloat where (!) = C.atIndex
-- instance C.Indexable (C.Vector CDouble) CDouble where (!) = C.atIndex

fst3 :: (a, b, c) -> a
fst3 (x, _, _) = x
