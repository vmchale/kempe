module Kempe.Inline ( kempeGraph
                    ) where

import           Data.Graph       (Graph, Vertex, graphFromEdges, path, reachable)
import qualified Data.IntMap      as IM
import           Data.Maybe       (fromMaybe, mapMaybe)
import           Data.Tuple.Extra (third3)
import           Kempe.AST
import           Kempe.Name
import           Kempe.Unique

-- | A 'FnModuleMap' is a map which retrives the 'Atoms's defining
-- a given 'Name'
type FnModuleMap c b = IM.IntMap [Atom c b]

inline :: Module a c b -> Module a c b
inline m = let fnMap = mkFnModuleMap m in fmap (inlineDecl fnMap) m
    where inlineDecl :: FnModuleMap c b -> KempeDecl a c b -> KempeDecl a c b
          inlineDecl = undefined

kempeGraph :: Module a c b -> (Graph, Vertex -> (KempeDecl a c b, Name b, [Name b]), Name b -> Vertex)
kempeGraph = third3 (findVtx .) . graphFromEdges . kempePreGraph
    where findVtx = fromMaybe (error "Internal error: bad name lookup!")

kempePreGraph :: Module a c b -> [(KempeDecl a c b, Name b, [Name b])]
kempePreGraph = mapMaybe kempeDeclToGraph
    where kempeDeclToGraph :: KempeDecl a c b -> Maybe (KempeDecl a c b, Name b, [Name b])
          kempeDeclToGraph d@(FunDecl _ n _ _ as) = Just (d, n, foldMap namesInAtom as)
          kempeDeclToGraph _                      = Nothing

mkFnModuleMap :: Module a c b -> FnModuleMap c b
mkFnModuleMap = IM.fromList . mapMaybe toInt where
    toInt (FunDecl _ (Name _ (Unique i) _) _ _ as) = Just (i, as)
    toInt _                                        = Nothing

namesInAtom :: Atom c a -> [Name a]
namesInAtom AtBuiltin{}   = []
namesInAtom (If _ as as') = foldMap namesInAtom as <> foldMap namesInAtom as'
namesInAtom (Dip _ as)    = foldMap namesInAtom as
namesInAtom (AtName _ n)  = [n]
namesInAtom AtCons{}      = []
namesInAtom IntLit{}      = []
namesInAtom BoolLit{}     = []
namesInAtom Int8Lit{}     = []
namesInAtom WordLit{}     = []
namesInAtom (Case _ as)   = foldMap namesInAtom (foldMap snd as)
