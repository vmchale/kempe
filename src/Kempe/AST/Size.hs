{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Frontend AST
-- | This module is split out so that the bakend/IR need not depend on
-- everything in 'AST'.
module Kempe.AST.Size ( KempeTy (..)
                      , StackType (..)
                      , MonoStackType
                      , BuiltinTy (..)
                      , ABI (..)
                      , prettyMonoStackType
                      -- * Sizing bits
                      , SizeEnv
                      , Size
                      , size
                      , size'
                      , sizeStack
                      ) where

import           Control.DeepSeq (NFData)
import           Data.Int        (Int64)
import qualified Data.IntMap     as IM
import           Data.Monoid     (Sum (..))
import qualified Data.Set        as S
import           GHC.Generics    (Generic)
import           Kempe.Name
import           Kempe.Unique
import           Prettyprinter   (Doc, Pretty (pretty), brackets, parens, sep, (<+>))

data KempeTy a = TyBuiltin { tyLoc :: a, tyBuiltin :: BuiltinTy }
               | TyNamed { tyLoc :: a, tyName :: TyName a }
               | TyVar { tyLoc :: a, var :: Name a }
               | TyApp { tyLoc :: a, tyCons :: KempeTy a, tyApp :: KempeTy a } -- type applied to another, e.g. Just Int
               | QuotTy { tyLoc :: a, quotIn :: [KempeTy a], quotOut :: [KempeTy a] }
               deriving (Generic, NFData, Functor, Eq, Ord) -- questionable eq instance but eh

data StackType b = StackType { quantify :: S.Set (Name b)
                             , inTypes  :: [KempeTy b]
                             , outTypes :: [KempeTy b]
                             } deriving (Generic, NFData, Eq, Ord)

type MonoStackType = ([KempeTy ()], [KempeTy ()])

prettyMonoStackType :: ([KempeTy a], [KempeTy a]) -> Doc ann
prettyMonoStackType (is, os) = sep (fmap pretty is) <+> "--" <+> sep (fmap pretty os)

data BuiltinTy = TyInt
               | TyBool
               | TyInt8
               | TyWord
               deriving (Generic, NFData, Eq, Ord)

instance Pretty BuiltinTy where
    pretty TyInt  = "Int"
    pretty TyBool = "Bool"
    pretty TyInt8 = "Int8"
    pretty TyWord = "Word"

instance Pretty (KempeTy a) where
    pretty (TyBuiltin _ b)     = pretty b
    pretty (TyNamed _ tn)      = pretty tn
    pretty (TyVar _ n)         = pretty n
    pretty (TyApp _ ty ty')    = parens (pretty ty <+> pretty ty')
    pretty (QuotTy _ tys tys') = brackets (curry prettyMonoStackType tys tys')

instance Pretty (StackType a) where
    pretty (StackType _ ins outs) = sep (fmap pretty ins) <+> "--" <+> sep (fmap pretty outs)

data ABI = Cabi
         | Kabi
         deriving (Eq, Ord, Generic, NFData)

instance Pretty ABI where
    pretty Cabi = "cabi"
    pretty Kabi = "kabi"

-- machinery for assigning a constructor to a function of its concrete types
-- (and then curry forward...)

type Size = [Int64] -> Int64
type SizeEnv = IM.IntMap Size

-- the kempe sizing system is kind of fucked (it works tho)

-- | Don't call this on ill-kinded types; it won't throw any error.
size :: SizeEnv -> KempeTy a -> Size
size _ (TyBuiltin _ TyInt)                 = const 8
size _ (TyBuiltin _ TyBool)                = const 1
size _ (TyBuiltin _ TyInt8)                = const 1
size _ (TyBuiltin _ TyWord)                = const 8
size _ TyVar{}                             = error "Internal error: type variables should not be present at this stage."
size env (TyNamed _ (Name _ (Unique k) _)) = IM.findWithDefault (error "Size not in map!") k env
size env (TyApp _ ty ty')                  = \tys -> size env ty (size env ty' [] : tys)
size _ QuotTy{}                            = const 8 -- just the pointer to itself

size' :: SizeEnv -> KempeTy a -> Int64
size' env = ($ []) . size env

sizeStack :: SizeEnv -> [KempeTy a] -> Int64
sizeStack env = getSum . foldMap (Sum . size' env)
