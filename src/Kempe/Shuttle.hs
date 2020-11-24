module Kempe.Shuttle ( monomorphize
                     ) where

import           Data.Bitraversable       (bitraverse)
import           Kempe.AST
import           Kempe.Error
import           Kempe.Monomorphize
import           Kempe.Monomorphize.Error
import           Kempe.TyAssign

monomorphize :: Int
             -> Module a c b
             -> Either (Error ()) (Module () (ConsAnn MonoStackType) MonoStackType)
monomorphize ctx m = do
    (mTy, i) <- runTypeM ctx (assignModule m)
    (flat, _) <- runMonoM i (flattenModule mTy)
    let flatFn' = filter (not . isTyDecl) flat
    traverse (bitraverse tryMonoConsAnn tryMono) flatFn'

isTyDecl :: KempeDecl a c b -> Bool
isTyDecl TyDecl{} = True
isTyDecl _        = False
