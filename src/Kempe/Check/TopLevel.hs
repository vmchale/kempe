module Kempe.Check.TopLevel ( topLevelCheck
                            , Warning
                            ) where

import           Control.Applicative ((<|>))
import           Data.Foldable       (toList)
import           Data.Foldable.Ext
import           Data.List           (group, sort)
import           Data.Maybe          (mapMaybe)
import           Kempe.AST
import           Kempe.Error.Warning
import           Kempe.Name

topLevelCheck :: Declarations a c a -> Maybe (Warning a)
topLevelCheck ds =
        checkNames (collectNames ds)
    <|> checkNames (collectCons ds)

-- | Just checks function names and type names. Doesn't check constructors.
collectNames :: Declarations a c a -> [Name a]
collectNames = mapMaybe collectDeclNames where
    collectDeclNames (FunDecl _ n _ _ _)   = Just n
    collectDeclNames (ExtFnDecl _ n _ _ _) = Just n
    collectDeclNames Export{}              = Nothing
    collectDeclNames (TyDecl _ tn _ _)     = Just tn

collectCons :: Declarations a c b-> [Name b]
collectCons = concatMap collectDeclNames where
    collectDeclNames (TyDecl _ _ _ ls) = toList (fst <$> ls)
    collectDeclNames _                 = []

checkNames :: [Name a] -> Maybe (Warning a)
checkNames ns = foldMapAlternative announce (group $ sort ns) -- maybe could be better idk
    where announce (_:y:_) = Just $ NameClash (loc y) y
          announce _       = Nothing
