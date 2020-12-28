{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Kempe.Asm.Type ( ControlAnn (..)
                      , Liveness (..)
                      ) where

import           Control.DeepSeq         (NFData)
import qualified Data.ByteString         as BS
import qualified Data.ByteString.Lazy    as BSL
import           Data.Copointed
import           Data.Int                (Int64, Int8)
import qualified Data.IntSet             as IS
import           Data.Semigroup          ((<>))
import           Data.Text.Encoding      (decodeUtf8)
import qualified Data.Text.Lazy.Encoding as TL
import           Data.Word               (Word8)
import           GHC.Generics            (Generic)
import           Prettyprinter           (Doc, Pretty (pretty), braces, brackets, colon, concatWith, hardline, indent, punctuate, (<+>))
import           Prettyprinter.Ext

data Liveness = Liveness { ins :: !IS.IntSet, out :: !IS.IntSet } -- strictness annotations make it perform better
    deriving (Eq, Generic, NFData)

instance Pretty Liveness where
    pretty (Liveness is os) = braces (pp is <+> ";" <+> pp os)
        where pp = mconcat . punctuate "," . fmap pretty . IS.toList

-- | Control-flow annotations
data ControlAnn = ControlAnn { node     :: !Int
                             , conn     :: [Int]
                             , usesNode :: IS.IntSet
                             , defsNode :: IS.IntSet
                             } deriving (Generic, NFData)
