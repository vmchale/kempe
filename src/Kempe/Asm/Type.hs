{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Kempe.Asm.Type ( ControlAnn (..)
                      , Liveness (..)
                      ) where

import           Control.DeepSeq (NFData)
import qualified Data.IntSet     as IS
import           Data.Semigroup  ((<>))
import           GHC.Generics    (Generic)
import           Prettyprinter   (Pretty (pretty), braces, punctuate, (<+>))

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
