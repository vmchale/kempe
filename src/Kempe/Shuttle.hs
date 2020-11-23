module Kempe.Shuttle ( monomorphize
                     ) where

import           Data.Bifunctor.Ext
import           Data.Bitraversable       (bitraverse)
import           Data.List                (partition)
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
    (flat, j) <- runMonoM i (flattenModule mTy)
    -- assign types again
    (flat', _) <- runTypeM j (assignModule flat)
    let (_, flatFn') = partition isTyDecl flat'
    -- save tydecls from flatten round (since they're annotated with types there
    -- already)
    traverse (bitraverse tryMonoConsAnn tryMono) (fmap (undefined ~<$) flatFn')

isTyDecl :: KempeDecl a c b -> Bool
isTyDecl TyDecl{} = True
isTyDecl _        = False
