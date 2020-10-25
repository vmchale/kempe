-- | IR from Appel book
module Kempe.IR ( size
                , stackPointer
                , framePointer
                , writeModule
                , Stmt (..)
                , Exp (..)
                , RelBinOp (..)
                , IntBinOp (..)
                ) where

import           Control.Monad.State  (State, gets, modify)
import qualified Data.ByteString.Lazy as BSL
import           Data.Foldable        (fold)
import           Data.Int             (Int64)
import qualified Data.IntMap          as IM
import           Data.Word            (Word8)
import           Kempe.AST
import           Kempe.Name
import           Kempe.Unique
import           Lens.Micro           (Lens')
import           Lens.Micro.Mtl       (modifying)

data Label

data Temp

class Architecture a where
    stackPointer :: a -> Temp
    framePointer :: a -> Temp

data TempSt = TempSt { labels     :: [Label]
                     , tempSupply :: [Temp]
                     , atLabels   :: IM.IntMap Label
                     -- TODO: type sizes in state?
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
data Stmt = Push Exp
          | Pop (KempeTy ()) Temp
          | Labeled Label
          -- -- | Seq Stmt Stmt
          | Jump Label
          -- conditional jump for ifs
          | CJump Exp Label Label
          | MJump Exp Label
          | CCall MonoStackType BSL.ByteString -- TODO: ShortByteString?
          | KCall Label -- KCall is a jump to a Kempe procedure (and jump back, later)
          | MovTemp Temp Exp
          | MovMem Exp Exp

data Exp = ConstInt Int64
         | ConstantPtr Int64
         | ConstBool Word8
         | Named Label
         | Reg Temp
         | Mem Exp
         | Do Stmt Exp
         | ExprIntBinOp IntBinOp Exp Exp
         | ExprIntRel RelBinOp Exp Exp

data RelBinOp = IntEq
              | IntNeq
              | IntLt
              | IntGt

data IntBinOp = IntPlus
              | IntTimes
              | IntDiv
              | IntMinus
              | IntMod

writeModule :: Module () MonoStackType -> TempM [Stmt]
writeModule = foldMapA writeDecl

writeDecl :: KempeDecl () MonoStackType -> TempM [Stmt]
writeDecl (FunDecl _ (Name _ u _) _ _ as) = do
    bl <- broadcastName u
    (Labeled bl:) <$> writeAtoms as

writeAtoms :: [Atom MonoStackType] -> TempM [Stmt]
writeAtoms = foldMapA writeAtom

foldMapA :: (Applicative f, Traversable t, Monoid m) => (a -> f m) -> t a -> f m
foldMapA = (fmap fold .) . traverse

-- need monad for fresh 'Temp's
writeAtom :: Atom MonoStackType -> TempM [Stmt]
writeAtom (IntLit _ i)             = pure [Push (ConstInt $ fromInteger i)]
writeAtom (BoolLit _ b)            = pure [Push (ConstBool $ toByte b)]
writeAtom (AtName _ n)             = pure . KCall <$> lookupName n -- TODO: when to do tco?
writeAtom (AtBuiltin ([], _) Drop) = error "Internal error: Ill-typed drop!"

toByte :: Bool -> Word8
toByte True  = 1
toByte False = 0

-- need env with size?
size :: KempeTy a -> Int
size (TyBuiltin _ TyInt)  = 8 -- since we're only targeting x86_64 and aarch64 we have 64-bit 'Int's
size (TyBuiltin _ TyPtr)  = 8
size (TyBuiltin _ TyBool) = 1
size TyVar{}              = error "Internal error: type variables should not be present at this stage."
size (TyTuple _ tys)      = sum (fmap size tys)
