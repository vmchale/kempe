module Kempe.CGen ( cGen
                  ) where

import           Data.Maybe     (mapMaybe)
import           Kempe.AST
import           Kempe.Name
import           Language.C.AST

cGen :: Declarations a c (StackType ()) -> [CFunc]
cGen = mapMaybe cDecl

cDecl :: KempeDecl a c (StackType ()) -> Maybe CFunc
cDecl ExtFnDecl{}                                        = Nothing
cDecl TyDecl{}                                           = Nothing
cDecl FunDecl{}                                          = Nothing
cDecl (Export _ Cabi (Name n _ (StackType _ [] [])))     = Just (CFunc n [CVoid] CVoid)
cDecl (Export _ Cabi (Name n _ (StackType _ [] [o])))    = Just (CFunc n [CVoid] (kempeTyToCType o))
cDecl (Export _ Cabi (Name n _ (StackType _ ins [])))    = Just (CFunc n (kempeTyToCType <$> ins) CVoid)
cDecl (Export _ Cabi (Name n _ (StackType _ ins [o])))   = Just (CFunc n (kempeTyToCType <$> ins) (kempeTyToCType o))
cDecl (Export _ Cabi _)                                  = error "Multiple return not suppported :("
cDecl (Export _ ArmAbi (Name n _ (StackType _ [] [])))   = Just (CFunc n [CVoidPtr] CVoid)
cDecl (Export _ ArmAbi (Name n _ (StackType _ [] [o])))  = Just (CFunc n [CVoidPtr] (kempeTyToCType o))
cDecl (Export _ ArmAbi (Name n _ (StackType _ ins [])))  = Just (CFunc n (CVoidPtr : fmap kempeTyToCType ins) CVoid)
cDecl (Export _ ArmAbi (Name n _ (StackType _ ins [o]))) = Just (CFunc n (CVoidPtr : fmap kempeTyToCType ins) (kempeTyToCType o))
cDecl (Export _ ArmAbi _)                                = error "Multiple return not suppported :("
cDecl (Export _ Hooked (Name n _ _))                     = Just (CFunc n [CVoidPtr] CVoid)
cDecl (Export _ Kabi _)                                  = error "You probably don't want to do this."

kempeTyToCType :: KempeTy a -> CType
kempeTyToCType (TyBuiltin _ TyInt)  = CInt
kempeTyToCType (TyBuiltin _ TyBool) = CBool
kempeTyToCType (TyBuiltin _ TyWord) = CUInt64
kempeTyToCType (TyBuiltin _ TyInt8) = CInt8
kempeTyToCType TyVar{}              = error "Don't do that"
kempeTyToCType TyApp{}              = error "User-defined types cannot be exported :("
kempeTyToCType TyNamed{}            = error "User-defined types cannot be exported :("
