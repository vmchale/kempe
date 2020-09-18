{-# LANGUAGE DeriveFunctor #-}

module Kempe.Name ( Name (..)
                  , TyName
                  ) where

import           Control.DeepSeq (NFData (..))
import qualified Data.Text       as T
import           Kempe.Unique

data Name a = Name { name   :: T.Text
                   , unique :: !Unique
                   , loc    :: a
                   } deriving (Functor)

instance Eq (Name a) where
    (==) (Name _ u _) (Name _ u' _) = u == u'

instance Ord (Name a) where
    compare (Name _ u _) (Name _ u' _) = compare u u'

instance NFData a => NFData (Name a) where
    rnf (Name _ u x) = rnf x `seq` u `seq` ()

type TyName = Name
