module Kempe.Shuttle ( monomorphize
                     ) where

import           Data.Bitraversable       (bitraverse)
import           Data.Functor             (void)
import           Kempe.AST
import           Kempe.Error
import           Kempe.Inline
import           Kempe.Monomorphize
import           Kempe.Monomorphize.Error
import           Kempe.TyAssign

inlineAssignFlatten :: Int
              -> Module a c b
              -> Either (Error ()) (Module () (ConsAnn (StackType ())) (StackType ()), Int)
inlineAssignFlatten ctx m = do
    -- check before inlining otherwise users would get weird errors
    void $ runTypeM ctx (checkModule m)
    (mTy, i) <- runTypeM ctx (assignModule $ inline m)
    runMonoM i (flattenModule mTy)

monomorphize :: Int
             -> Module a c b
             -> Either (Error ()) (Module () (ConsAnn MonoStackType) MonoStackType)
monomorphize ctx m = do
    (flat, _) <- inlineAssignFlatten ctx m
    let flatFn' = filter (not . isTyDecl) flat
    traverse (bitraverse tryMonoConsAnn tryMono) flatFn'

isTyDecl :: KempeDecl a c b -> Bool
isTyDecl TyDecl{} = True
isTyDecl _        = False
