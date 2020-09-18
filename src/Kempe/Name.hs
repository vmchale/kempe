{-# LANGUAGE DeriveFunctor #-}

module Kempe.Name ( Name (..)
                  , TyName
                  ) where

import qualified Data.Text    as T
import           Kempe.Unique

data Name a = Name { name   :: T.Text
                   , unique :: !Unique
                   , loc    :: a
                   } deriving (Functor)

instance Eq (Name a) where
    (==) (Name _ u _) (Name _ u' _) = u == u'

instance Ord (Name a) where
    compare (Name _ u _) (Name _ u' _) = compare u u'

type TyName = Name
