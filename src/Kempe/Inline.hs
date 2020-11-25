module Kempe.Inline ( inline
                    ) where

import           Data.Graph       (Graph, Vertex, graphFromEdges, path)
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
inline m = fmap inlineDecl m
    where inlineDecl (FunDecl l n ty ty' as) = FunDecl l n ty ty' (inlineAtoms n as)
          inlineDecl d                       = d
          inlineAtoms n = concatMap (inlineAtom n)
          inlineAtom declName a@(AtName _ n) =
            if path graph (nLookup n) (nLookup declName) -- FIXME: criterion allows things which are recursive (i.e. inline isPrimeStep more than once in isPrime)
                -- need to mark things which are recursive?
                then [a] -- no inline
                else findDecl (unUnique (unique n)) fnMap
          inlineAtom declName (If l as as') =
            [If l (inlineAtoms declName as) (inlineAtoms declName as')]
          inlineAtom _ Case{} = undefined
          inlineAtom _ a = [a]
          fnMap = mkFnModuleMap m
          (graph, _, nLookup) = kempeGraph m
          findDecl = IM.findWithDefault (error "Internal error: FnModuleMap does not contain name/declaration!")

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
    toInt (FunDecl _ (Name _ (Unique i) _) _ _ as) = Just (i, as) -- FIXME: this gets rid of external function decls, when it should mark them
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
