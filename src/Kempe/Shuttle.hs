module Kempe.Shuttle ( monomorphize
                     ) where

import           Data.Bitraversable       (bitraverse)
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
    -- TODO: typecheck
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
