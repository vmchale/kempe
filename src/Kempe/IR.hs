module Kempe.IR ( size
                , stackPointer
                , writeDecl
                , Statement (..)
                , Expression (..)
                , RelBinOp (..)
                , IntBinOp (..)
                ) where

import           Control.Monad.State (State, gets, modify)
import           Data.Foldable       (fold)
import           Data.Int            (Int64)
import qualified Data.IntMap         as IM
import           Data.Word           (Word8)
import           Kempe.AST
import           Kempe.Name
import           Kempe.Unique
import           Lens.Micro          (Lens')
import           Lens.Micro.Mtl      (modifying)

data Label

data Temp

-- defined on aarch64 and x86_64
stackPointer :: Temp
stackPointer = undefined

data TempSt = TempSt { labels     :: [Label]
                     , tempSupply :: [Temp]
                     , atLabels   :: IM.IntMap Label
                     }

atLabelsLens :: Lens' TempSt (IM.IntMap Label)
atLabelsLens f s = fmap (\x -> s { atLabels = x }) (f (atLabels s))

mapLabels :: ([Label] -> [Label]) -> TempSt -> TempSt
mapLabels f (TempSt ls ts ats) = TempSt (f ls) ts ats

mapTemps :: ([Temp] -> [Temp]) -> TempSt -> TempSt
mapTemps f (TempSt ls ts ats) = TempSt ls (f ts) ats

type TempM = State TempSt

getTemp :: TempM Temp
getTemp = gets (head . tempSupply) <* modify (mapTemps tail)

newLabel :: TempM Label
newLabel = gets (head . labels) <* modify (mapLabels tail)

broadcastName :: Unique -> TempM Label
broadcastName (Unique i) = do
    l <- newLabel
    modifying atLabelsLens (IM.insert i l)
    pure l

lookupName :: Name a -> TempM Label
lookupName (Name _ (Unique i) _) =
    gets
        (IM.findWithDefault (error "Internal error in IR phase: could not look find label for name") i . atLabels)

-- TODO figure out dip
data Statement = Push Expression
               | Pop (KempeTy ()) Temp
               | Labeled Label
               -- -- | Seq Statement Statement
               | Jump Label
               | CJump Expression Label Label
               -- -- | CCall MonoStackType

data Expression = ConstInt Int64
                | ConstantPtr Int64
                | ConstBool Word8
                | Named Label
                | Reg Temp
                | Do Statement Expression
                | ExprIntBinOp IntBinOp Expression Expression
                | ExprIntRel RelBinOp Expression Expression

data RelBinOp = IntEq
              | IntNeq
              | IntLt
              | IntGt

data IntBinOp = IntPlus
              | IntTimes
              | IntDiv
              | IntMinus
              | IntMod

writeDecl :: KempeDecl () MonoStackType -> TempM [Statement]
writeDecl (FunDecl _ (Name _ u _) _ _ as) = do
    bl <- broadcastName u
    (Labeled bl:) <$> writeAtoms as

writeAtoms :: [Atom MonoStackType] -> TempM [Statement]
writeAtoms = foldMapA writeAtom where
    foldMapA = (fmap fold .) . traverse

-- need monad for fresh 'Temp's
writeAtom :: Atom MonoStackType -> TempM [Statement]
writeAtom (IntLit _ i)  = pure [Push (ConstInt $ fromInteger i)]
writeAtom (BoolLit _ b) = pure [Push (ConstBool $ toByte b)]
writeAtom (AtName _ n)  = pure . Jump <$> lookupName n -- TODO: when to do tco?

toByte :: Bool -> Word8
toByte True  = 1
toByte False = 0

-- need env with size?
size :: KempeTy a -> Int
size (TyBuiltin _ TyInt)  = 8 -- since we're only targeting x86_64 and aarch64 we have 64-bit 'Int's
size (TyBuiltin _ TyPtr)  = 8
size (TyBuiltin _ TyBool) = 1
size TyVar{}              = error "Internal error: type variables not allowed at this stage."
size (TyTuple _ tys)      = sum (fmap size tys)
