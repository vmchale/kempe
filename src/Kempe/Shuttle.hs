module Kempe.Shuttle ( monomorphize
                     ) where

import           Kempe.AST
import           Kempe.Error
import           Kempe.Specialize
import           Kempe.TyAssign

monomorphize :: Int
             -> Module a b
             -> Either (Error ()) (Module () MonoStackType)
monomorphize ctx m = do
    (mTy, i) <- runTypeM ctx (assignModule m)
    (flat, j) <- runMonoM i (flattenModule mTy)
    -- assign types again lol
    (flatTy, _) <- runTypeM j (assignModule flat)
    traverse (traverse tryMono) flatTy
