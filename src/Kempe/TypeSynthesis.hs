{-# LANGUAGE OverloadedStrings #-}

module Kempe.TypeSynthesis ( TypeM
                           ) where

import           Control.Monad.State
import qualified Data.IntMap         as IM
import qualified Data.Set            as S
import qualified Data.Text           as T
import           Kempe.AST
import           Kempe.Name
import           Kempe.Unique
import           Lens.Micro          (Lens')
import           Lens.Micro.Mtl      (modifying)

type TyEnv a = IM.IntMap (KempeTy a)

data TyState a = TyState { maxU             :: Int -- ^ For renamer
                         , tyEnv            :: TyEnv a
                         , renames          :: IM.IntMap Int
                         , constructorTypes :: IM.IntMap Int
                         }

maxULens :: Lens' (TyState a) Int
maxULens f s = fmap (\x -> s { maxU = x }) (f (maxU s))

dummyName :: T.Text -> TypeM () (Name ())
dummyName n = do
    pSt <- gets maxU
    Name n (Unique $ pSt + 1) ()
        <$ modifying maxULens (+1)

type TypeM a = State (TyState a)

-- TODO: take constructor types as an argument?..
runTypeM :: TypeM a x -> x
runTypeM = flip evalState (TyState 0 mempty mempty mempty)

-- alpha-equivalence (of 'StackType's?) (note it is quantified *only* on the "exterior" i.e.
-- implicitly) -> except we have to then "back-instantiate"? hm

-- monomorphization

-- dip-ify?

-- renameStackType? or maybe j substitute?

typeOfBuiltin :: BuiltinFn -> TypeM () (StackType ())
typeOfBuiltin Drop = do
    aN <- dummyName "a"
    pure $ StackType (S.singleton aN) [TyVar () aN] []
typeOfBuiltin Swap = do
    aN <- dummyName "a"
    bN <- dummyName "b"
    pure $ StackType (S.fromList [aN, bN]) [TyVar () aN, TyVar () bN] [TyVar () bN, TyVar () aN]

-- maybe constraints? e.g. ("a" = "b") and (3 = "a")
-- but maybe simpler since no function types? lol
--
-- so I can literally just check it's 3 and then pass that back lololol

-- | Given @x@ and @y@, return the 'StackType' of @x y@
catTypes :: StackType a -- ^ @x@
         -> StackType a -- ^ @y@
         -> StackType a
catTypes _ _ = undefined -- I need unification? :o
