module Kempe.Shuttle ( monomorphize
                     ) where

import           Data.Bitraversable (bitraverse)
import           Data.List          (partition)
import           Kempe.AST
import           Kempe.Error
import           Kempe.Monomorphize
import           Kempe.TyAssign

monomorphize :: Int
             -> Module a c b
             -> Either (Error ()) (Module () MonoStackType MonoStackType)
monomorphize ctx m = do
    (mTy, i) <- runTypeM ctx (assignModule m)
    (flat, j) <- runMonoM i (flattenModule mTy)
    let (flatTy, _) = partition isTyDecl flat
    -- assign types again
    (flat', _) <- runTypeM j (assignModule flat)
    let (_, flatFn') = partition isTyDecl flat'
    -- save tydecls from flatten round (since they're annotated with types there
    -- already)
    traverse (bitraverse tryMono tryMono) (flatTy ++ flatFn')

isTyDecl :: KempeDecl a c b -> Bool
isTyDecl TyDecl{} = True
isTyDecl _        = False
