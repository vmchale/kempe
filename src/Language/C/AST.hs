{-# LANGUAGE OverloadedStrings #-}

module Language.C.AST ( CType (..)
                      , CFunc (..)
                      ) where

import qualified Data.Set      as S
import qualified Data.Text     as T
import           Prettyprinter (Pretty (..))

data CHeaders = StdBool -- ^ @stdbool.h@
              | StdInt -- ^ @stdint.h@

data CType = CInt
           | CBool
           | CUInt64
           -- ADTs etc.

data CFunc = CFunc !T.Text [CType] CType

instance Pretty CType where
    pretty CInt    = "int"
    pretty CBool   = "bool"
    pretty CUInt64 = "uint64_t"
