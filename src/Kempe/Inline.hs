-- | A simple inliner. Inlines all non-recursive functions.
--
-- This should all work.
module Kempe.Inline ( inline
                    ) where

import           Data.Graph         (Graph, Vertex, graphFromEdges, path)
import qualified Data.IntMap        as IM
import qualified Data.List.NonEmpty as NE
import           Data.Maybe         (fromMaybe, mapMaybe)
import           Data.Semigroup     ((<>))
import           Data.Tuple.Extra   (third3)
import           Kempe.AST
import           Kempe.Name
import           Kempe.Unique

-- | A 'FnModuleMap' is a map which retrives the 'Atoms's defining
-- a given 'Name'
type FnModuleMap c b = IM.IntMap (Maybe [Atom c b])

inline :: Declarations a c b -> Declarations a c b
inline m = fmap inlineDecl m
    where inlineDecl (FunDecl l n ty ty' as) = FunDecl l n ty ty' (inlineAtoms n as)
          inlineDecl d                       = d
          inlineAtoms n = concatMap (inlineAtom n)
          inlineAtom declName a@(AtName _ n) =
            if path graph (nLookup n) (nLookup declName) || don'tInline n
                then [a] -- no inline
                else foldMap (inlineAtom declName) $ findDecl a n
          inlineAtom declName (If l as as') =
            [If l (inlineAtoms declName as) (inlineAtoms declName as')]
          inlineAtom declName (Case l ls) =
            let (ps, ass) = NE.unzip ls
                in [Case l (NE.zip ps $ fmap (inlineAtoms declName) ass)]
          inlineAtom _ a = [a]
          fnMap = mkFnModuleMap m
          (graph, _, nLookup) = kempeGraph m
          findDecl at (Name _ (Unique k) _) =
            case findPreDecl k fnMap of
                Just as -> as
                Nothing -> pure at -- tried to inline an extern function
          findPreDecl = IM.findWithDefault (error "Internal error: FnModuleMap does not contain name/declaration!")
          recMap = graphRecursiveMap m (graph, nLookup)
          don'tInline (Name _ (Unique i) _) = IM.findWithDefault (error "Internal error! recursive map missing key!") i recMap

-- | Given a module, make a map telling which top-level names are recursive or
-- cannot be inlined
graphRecursiveMap :: Declarations a c b -> (Graph, Name b -> Vertex) -> IM.IntMap Bool
graphRecursiveMap m (graph, nLookup) = IM.fromList $ mapMaybe fnRecursive m
    where fnRecursive (FunDecl _ n@(Name _ (Unique i) _) _ _ as) | n `elem` namesInAtoms as = Just (i, True) -- if it calls iteself
                                                                 | anyReachable n as = Just (i, True)
                                                                 | otherwise = Just (i, False)
          fnRecursive (ExtFnDecl _ (Name _ (Unique i) _) _ _ _) = Just (i, True) -- not recursive but don't try to inline this
          fnRecursive _ = Nothing
          anyReachable n as =
            any (\nA -> path graph (nLookup nA) (nLookup n)) (namesInAtoms as) -- TODO: lift let-binding (nLookup?)


kempeGraph :: Declarations a c b -> (Graph, Vertex -> (KempeDecl a c b, Name b, [Name b]), Name b -> Vertex)
kempeGraph = third3 (findVtx .) . graphFromEdges . kempePreGraph
    where findVtx = fromMaybe (error "Internal error: bad name lookup!")

kempePreGraph :: Declarations a c b -> [(KempeDecl a c b, Name b, [Name b])]
kempePreGraph = mapMaybe kempeDeclToGraph
    where kempeDeclToGraph :: KempeDecl a c b -> Maybe (KempeDecl a c b, Name b, [Name b])
          kempeDeclToGraph d@(FunDecl _ n _ _ as)  = Just (d, n, foldMap namesInAtom as)
          kempeDeclToGraph d@(ExtFnDecl _ n _ _ _) = Just (d, n, [])
          kempeDeclToGraph _                       = Nothing

mkFnModuleMap :: Declarations a c b -> FnModuleMap c b
mkFnModuleMap = IM.fromList . mapMaybe toInt where
    toInt (FunDecl _ (Name _ (Unique i) _) _ _ as)  = Just (i, Just as)
    toInt (ExtFnDecl _ (Name _ (Unique i) _) _ _ _) = Just (i, Nothing)
    toInt _                                         = Nothing

namesInAtoms :: [Atom c a] -> [Name a]
namesInAtoms = foldMap namesInAtom

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
