-- | The mapping between types that can be stored in a NetCDF file and
-- the FFI functions needed to read and write those values is
-- maintained by the `NcStorable` type class.

module Data.NetCDF.Storable
       ( NcStorable (..)
       ) where

import Foreign.C
import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal

import Data.NetCDF.Types
import Data.NetCDF.Raw.PutGet

import qualified Data.Vector.Storable as V
import Foreign.ForeignPtr

-- | Class to collect the NetCDF FFI functions needed to read and
-- write values in a NetCDF file for a given type.
class Storable a => NcStorable a where
  ncType :: a -> NcType
  ffi_put_var1 :: CInt -> CInt -> Ptr CULong -> Ptr a -> IO CInt
  ffi_get_var1 :: CInt -> CInt -> Ptr CULong -> Ptr a -> IO CInt
  ffi_put_var :: CInt -> CInt -> Ptr a -> Int -> IO CInt
  ffi_get_var :: CInt -> CInt -> Ptr a -> Int -> IO CInt
  ffi_put_vara :: CInt -> CInt -> Ptr CULong -> Ptr CULong -> Ptr a -> Int -> IO CInt
  ffi_get_vara :: CInt -> CInt -> Ptr CULong -> Ptr CULong -> Ptr a -> Int -> IO CInt
  ffi_put_vars :: CInt -> CInt -> Ptr CULong -> Ptr CULong -> Ptr CULong
               -> Ptr a -> Int -> IO CInt
  ffi_get_vars :: CInt -> CInt -> Ptr CULong -> Ptr CULong -> Ptr CULong
               -> Ptr a -> Int -> IO CInt


instance NcStorable CSChar where
  -- #define NC_BYTE 1 /**< signed 1 byte integer */
  ncType _ = NcByte
  ffi_put_var1 = nc_put_var1_schar'_
  ffi_get_var1 = nc_get_var1_schar'_
  ffi_put_var a b c _ = nc_put_var_schar'_ a b c
  ffi_get_var a b c _ = nc_get_var_schar'_ a b c
  ffi_put_vara a b c d e _ = nc_put_vara_schar'_ a b c d e
  ffi_get_vara a b c d e _ = nc_get_vara_schar'_ a b c d e
  ffi_put_vars a b c d e f _ = nc_put_vars_schar'_ a b c d e f
  ffi_get_vars a b c d e f _ = nc_get_vars_schar'_ a b c d e f

instance NcStorable CChar where
  -- #define NC_CHAR 2 /**< ISO/ASCII character */
  ncType _ = NcChar
  ffi_put_var1 = nc_put_var1_text'_
  ffi_get_var1 = nc_get_var1_text'_
  ffi_put_var a b c _ = nc_put_var_text'_ a b c
  ffi_get_var a b c _ = nc_get_var_text'_ a b c
  ffi_put_vara a b c d e _ = nc_put_vara_text'_ a b c d e
  ffi_get_vara a b c d e _ = nc_get_vara_text'_ a b c d e
  ffi_put_vars a b c d e f _ = nc_put_vars_text'_ a b c d e f
  ffi_get_vars a b c d e f _ = nc_get_vars_text'_ a b c d e f

instance NcStorable CShort where
  -- #define NC_SHORT 3 /**< signed 2 byte integer */
  ncType _ = NcShort
  ffi_put_var1 = nc_put_var1_short'_
  ffi_get_var1 = nc_get_var1_short'_
  ffi_put_var a b c _ = nc_put_var_short'_ a b c
  ffi_get_var a b c _ = nc_get_var_short'_ a b c
  ffi_put_vara a b c d e _ = nc_put_vara_short'_ a b c d e
  ffi_get_vara a b c d e _ = nc_get_vara_short'_ a b c d e
  ffi_put_vars a b c d e f _ = nc_put_vars_short'_ a b c d e f
  ffi_get_vars a b c d e f _ = nc_get_vars_short'_ a b c d e f

instance NcStorable CInt where
  -- #define NC_INT 4 /**< signed 4 byte integer */
  ncType _ = NcInt
  ffi_put_var1 = nc_put_var1_int'_
  ffi_get_var1 = nc_get_var1_int'_
  ffi_put_var a b c _ = nc_put_var_int'_ a b c
  ffi_get_var a b c _ = nc_get_var_int'_ a b c
  ffi_put_vara a b c d e _ = nc_put_vara_int'_ a b c d e
  ffi_get_vara a b c d e _ = nc_get_vara_int'_ a b c d e
  ffi_put_vars a b c d e f _ = nc_put_vars_int'_ a b c d e f
  ffi_get_vars a b c d e f _ = nc_get_vars_int'_ a b c d e f

instance NcStorable CFloat where
  -- #define NC_FLOAT 5 /**< single precision floating point number */
  ncType _ = NcFloat
  ffi_put_var1 = nc_put_var1_float'_
  ffi_get_var1 = nc_get_var1_float'_
  ffi_put_var a b c _ = nc_put_var_float'_ a b c
  ffi_get_var a b c _ = nc_get_var_float'_ a b c
  ffi_put_vara a b c d e _ = nc_put_vara_float'_ a b c d e
  ffi_get_vara a b c d e _ = nc_get_vara_float'_ a b c d e
  ffi_put_vars a b c d e f _ = nc_put_vars_float'_ a b c d e f
  ffi_get_vars a b c d e f _ = nc_get_vars_float'_ a b c d e f

instance NcStorable CDouble where
  -- #define NC_DOUBLE 6 /**< double precision floating point number */
  ncType _ = NcDouble
  ffi_put_var1 = nc_put_var1_double'_
  ffi_get_var1 = nc_get_var1_double'_
  ffi_put_var a b c _ = nc_put_var_double'_ a b c
  ffi_get_var a b c _ = nc_get_var_double'_ a b c
  ffi_put_vara a b c d e _ = nc_put_vara_double'_ a b c d e
  ffi_get_vara a b c d e _ = nc_get_vara_double'_ a b c d e
  ffi_put_vars a b c d e f _ = nc_put_vars_double'_ a b c d e f
  ffi_get_vars a b c d e f _ = nc_get_vars_double'_ a b c d e f

--
-- In hmatrix (<= 0.17.0.2) CFloat, and CDouble are not in class Element
-- and most hmatrix functions cannot deal with CFloat and CDouble
--
instance NcStorable Float where
  ncType _ = NcFloat
  ffi_put_var1 a b c d = do v <- peek d
                            with (realToFrac v :: CFloat) $ \d' -> nc_put_var1_float'_ a b c d'
  ffi_get_var1 a b c d = alloca $ \d' -> do r <- nc_get_var1_float'_ a b c d'
                                            v <- peek d'
                                            poke d (realToFrac v)
                                            return r
  ffi_put_var a b c n = do v <- mapM (\n' -> peekElemOff c n') [0 .. (n-1)]
                           let v' = map realToFrac v :: [CFloat]
                               vp = fst $ V.unsafeToForeignPtr0 $ V.fromList v'
                           withForeignPtr vp $ \isp -> nc_put_var_float'_ a b isp

  ffi_get_var a b c n = do isp <- mallocArray n
                           r <- nc_get_var_float'_ a b isp
                           v <- peekArray n isp
                           let v' = map realToFrac v :: [Float]
                           pokeArray c v'
                           free isp
                           return r

  ffi_put_vara a b c d e n = do v <- mapM (\n' -> peekElemOff e n') [0 .. (n-1)]
                                let v' = map realToFrac v :: [CFloat]
                                    vp = fst $ V.unsafeToForeignPtr0 $ V.fromList v'
                                withForeignPtr vp $ \isp -> nc_put_vara_float'_ a b c d isp

  ffi_get_vara a b c d e n = do isp <- mallocArray n
                                r <- nc_get_vara_float'_ a b c d isp
                                v <- peekArray n isp
                                let v' = map realToFrac v :: [Float]
                                pokeArray e v'
                                free isp
                                return r

  ffi_put_vars a b c d e f n = do v <- mapM (\n' -> peekElemOff f n') [0 .. (n-1)]
                                  let v' = map realToFrac v :: [CFloat]
                                      vp = fst $ V.unsafeToForeignPtr0 $ V.fromList v'
                                  withForeignPtr vp $ \isp -> nc_put_vars_float'_ a b c d e isp

  ffi_get_vars a b c d e f n = do isp <- mallocArray n
                                  r <- nc_get_vars_float'_ a b c d e isp
                                  v <- peekArray n isp
                                  let v' = map realToFrac v :: [Float]
                                  pokeArray f v'
                                  free isp
                                  return r

instance NcStorable Double where
  ncType _ = NcDouble
  ffi_put_var1 a b c d = do v <- peek d
                            with (realToFrac v :: CDouble) $ \d' -> nc_put_var1_double'_ a b c d'
  ffi_get_var1 a b c d = alloca $ \d' -> do r <- nc_get_var1_double'_ a b c d'
                                            v <- peek d'
                                            poke d (realToFrac v)
                                            return r
  ffi_put_var a b c n = do v <- mapM (\n' -> peekElemOff c n') [0 .. (n-1)]
                           let v' = map realToFrac v :: [CDouble]
                               vp = fst $ V.unsafeToForeignPtr0 $ V.fromList v'
                           withForeignPtr vp $ \isp -> nc_put_var_double'_ a b isp

  ffi_get_var a b c n = do isp <- mallocArray n
                           r <- nc_get_var_double'_ a b isp
                           v <- peekArray n isp
                           let v' = map realToFrac v :: [Double]
                           pokeArray c v'
                           free isp
                           return r

  ffi_put_vara a b c d e n = do v <- mapM (\n' -> peekElemOff e n') [0 .. (n-1)]
                                let v' = map realToFrac v :: [CDouble]
                                    vp = fst $ V.unsafeToForeignPtr0 $ V.fromList v'
                                withForeignPtr vp $ \isp -> nc_put_vara_double'_ a b c d isp

  ffi_get_vara a b c d e n = do isp <- mallocArray n
                                r <- nc_get_vara_double'_ a b c d isp
                                v <- peekArray n isp
                                let v' = map realToFrac v :: [Double]
                                pokeArray e v'
                                free isp
                                return r

  ffi_put_vars a b c d e f n = do v <- mapM (\n' -> peekElemOff f n') [0 .. (n-1)]
                                  let v' = map realToFrac v :: [CDouble]
                                      vp = fst $ V.unsafeToForeignPtr0 $ V.fromList v'
                                  withForeignPtr vp $ \isp -> nc_put_vars_double'_ a b c d e isp

  ffi_get_vars a b c d e f n = do isp <- mallocArray n
                                  r <- nc_get_vars_double'_ a b c d e isp
                                  v <- peekArray n isp
                                  let v' = map realToFrac v :: [Double]
                                  pokeArray f v'
                                  free isp
                                  return r
