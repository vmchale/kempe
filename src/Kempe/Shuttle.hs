{-# LANGUAGE TupleSections #-}

module Kempe.Shuttle ( monomorphize
                     ) where

import           Data.Functor        (void)
import           Kempe.AST
import           Kempe.Check.Pattern
import           Kempe.Error
import           Kempe.Inline
import           Kempe.Monomorphize
import           Kempe.TyAssign

inlineAssignFlatten :: Int
                    -> Declarations a c b
                    -> Either (Error ()) (Declarations () (ConsAnn MonoStackType) (StackType ()), (Int, SizeEnv))
inlineAssignFlatten ctx m = do
    -- check before inlining otherwise users would get weird errors
    void $ do
        void $ runTypeM ctx (checkModule m)
        mErr $ checkModuleExhaustive (void <$> m)
    (mTy, i) <- runTypeM ctx (assignModule $ inline m)
    runMonoM i (flattenModule mTy)

monomorphize :: Int
             -> Declarations a c b
             -> Either (Error ()) (Declarations () (ConsAnn MonoStackType) MonoStackType, SizeEnv)
monomorphize ctx m = do
    (flat, (_, env)) <- inlineAssignFlatten ctx m
    let flatFn' = filter (not . isTyDecl) flat
    (, env) <$> traverse (traverse tryMono) flatFn'

isTyDecl :: KempeDecl a c b -> Bool
isTyDecl TyDecl{} = True
isTyDecl _        = False
