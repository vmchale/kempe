module Kempe.Haskell.FFI ( ffiDo
                         ) where

import           Foreign.Ptr (FunPtr, Ptr)

foreign import ccall "dynamic" ffiDo :: FunPtr (Ptr a -> IO ()) -> (Ptr a -> IO ())
