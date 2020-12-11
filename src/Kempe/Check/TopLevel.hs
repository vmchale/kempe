{-# LANGUAGE OverloadedStrings #-}

module Kempe.Check.TopLevel ( topLevelCheck
                            , Warning
                            ) where

import           Control.Exception (Exception)
import           Data.Foldable.Ext
import           Data.List         (group, sort)
import           Data.Maybe        (mapMaybe)
import           Data.Typeable     (Typeable)
import           Kempe.AST
import           Kempe.Name
import           Prettyprinter     (Pretty (pretty))

data Warning a = NameClash a (Name a)

instance Pretty a => Pretty (Warning a) where
    pretty (NameClash l x) = pretty l <> ": '" <> pretty x <> "' is defined more than once."

topLevelCheck :: Declarations a c a -> Maybe (Warning a)
topLevelCheck = checkNames . collectNames

-- | Just checks function names and type names. Doesn't check constructors.
collectNames :: Declarations a c a -> [Name a]
collectNames = mapMaybe collectDeclNames where
    collectDeclNames (FunDecl _ n _ _ _)   = Just n
    collectDeclNames (ExtFnDecl _ n _ _ _) = Just n
    collectDeclNames Export{}              = Nothing
    collectDeclNames (TyDecl _ tn _ _)     = Just tn

checkNames :: [Name a] -> Maybe (Warning a)
checkNames ns = foldMapAlternative announce (group $ sort ns) -- maybe could be better idk
    where announce (_:y:_) = Just $ NameClash (loc y) y
          announce _       = Nothing

instance (Pretty a) => Show (Warning a) where
    show = show . pretty

instance (Pretty a, Typeable a) => Exception (Warning a)
