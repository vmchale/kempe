-- | Check pattern match exhaustiveness, since we don't really handle that in
-- source code.
--
-- This is pretty easy because of how patterns work in Kempe.
--
-- Some of this code is from Dickinson, but we don't need the Maranget approach
-- because the pattern matching is simpler in Kempe.
module Kempe.Check.Pattern ( checkModuleExhaustive
                           ) where

import           Control.Monad              (forM_)
import           Control.Monad.State.Strict (State, execState)
import           Data.Coerce                (coerce)
import           Data.Foldable              (toList, traverse_)
import           Data.Foldable.Ext
import qualified Data.IntMap.Strict         as IM
import qualified Data.IntSet                as IS
import           Data.List.NonEmpty         (NonEmpty (..))
import           Kempe.AST
import           Kempe.Error
import           Kempe.Name
import           Kempe.Unique
import           Lens.Micro                 (Lens')
import           Lens.Micro.Mtl             (modifying)

checkAtom :: PatternEnv -> Atom c b -> Maybe (Error b)
checkAtom env (Case l ls) =
    if isExhaustive env $ fmap fst ls
        then Nothing
        else Just (InexhaustiveMatch l)
checkAtom _ _ = Nothing

checkDecl :: PatternEnv -> KempeDecl a c b -> Maybe (Error b)
checkDecl env (FunDecl _ _ _ _ as) = foldMapAlternative (checkAtom env) as
checkDecl _ _                      = Nothing

checkModule :: PatternEnv -> Declarations a c b -> Maybe (Error b)
checkModule env = foldMapAlternative (checkDecl env)

checkModuleExhaustive :: Declarations a c b -> Maybe (Error b)
checkModuleExhaustive m =
    let env = runPatternM $ patternEnvDecls m
        in checkModule env m

data PatternEnv = PatternEnv { allCons :: IM.IntMap IS.IntSet -- ^ all constructors indexed by type
                             , types   :: IM.IntMap Int -- ^ all types indexed by constructor
                             }

allConsLens :: Lens' PatternEnv (IM.IntMap IS.IntSet)
allConsLens f s = fmap (\x -> s { allCons = x }) (f (allCons s))

typesLens :: Lens' PatternEnv (IM.IntMap Int)
typesLens f s = fmap (\x -> s { types = x }) (f (types s))

type PatternM = State PatternEnv

patternEnvDecls :: Declarations a c b -> PatternM ()
patternEnvDecls = traverse_ declAdd

declAdd :: KempeDecl a c b -> PatternM ()
declAdd FunDecl{}                             = pure ()
declAdd ExtFnDecl{}                           = pure ()
declAdd Export{}                              = pure ()
declAdd (TyDecl _ (Name _ (Unique i) _) _ ls) = do
    forM_ ls $ \(Name _ (Unique j) _, _) ->
        modifying typesLens (IM.insert j i)
    let cons = IS.fromList $ toList (unUnique . unique . fst <$> ls)
    modifying allConsLens (IM.insert i cons)

runPatternM :: PatternM a -> PatternEnv
runPatternM = flip execState (PatternEnv mempty mempty)

internalError :: a
internalError = error "Internal error: lookup in a PatternEnv failed"

-- given a constructor name, get the IntSet of all constructors of that type
assocUniques :: PatternEnv -> Name a -> IS.IntSet
assocUniques env (Name _ (Unique i) _) =
    let ty = IM.findWithDefault internalError i (types env)
        in IM.findWithDefault internalError ty (allCons env)

hasWildcard :: Foldable t => t (Pattern c b) -> Bool
hasWildcard = any isWildcard where
    isWildcard PatternWildcard{} = True
    isWildcard _                 = False

-- | Only works on well-typed stuff
isExhaustive :: PatternEnv -> NonEmpty (Pattern c b) -> Bool
isExhaustive _ (PatternWildcard{}:|_)                      = True
isExhaustive _ (PatternInt{}:|ps)                          = hasWildcard ps
isExhaustive _ (PatternBool _ True:|PatternBool _ False:_) = True
isExhaustive _ (PatternBool _ False:|PatternBool _ True:_) = True
isExhaustive _ (PatternBool{}:|ps)                         = hasWildcard ps
isExhaustive env ps@(PatternCons{}:|_)                     = isCompleteSet env (fmap patternName ps)

isCompleteSet :: PatternEnv -> NonEmpty (TyName a) -> Bool
isCompleteSet env ns@(n:|_) =
    let allU = assocUniques env n
        ty = coerce (unique <$> toList ns)
        in IS.null (allU IS.\\ IS.fromList ty)
