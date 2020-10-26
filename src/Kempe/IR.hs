-- | IR from Appel book
module Kempe.IR ( size
                , Architecture (..)
                , writeModule
                , Stmt (..)
                , Exp (..)
                , RelBinOp (..)
                , IntBinOp (..)
                , runTempM
                , TempM
                ) where

import           Control.Monad.State  (State, evalState, gets, modify)
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

type Label = Word

type Temp = Int

data Architecture = Architecture { stackPointer :: Temp
                                 , framePointer :: Temp
                                 , cRet :: Exp
                                 }

data TempSt = TempSt { labels     :: [Label]
                     , tempSupply :: [Temp]
                     , atLabels   :: IM.IntMap Label
                     -- TODO: type sizes in state
                     }

runTempM :: TempM a -> a
runTempM = flip evalState (TempSt [1..] [1..] mempty)

atLabelsLens :: Lens' TempSt (IM.IntMap Label)
atLabelsLens f s = fmap (\x -> s { atLabels = x }) (f (atLabels s))

nextLabels :: TempSt -> TempSt
nextLabels (TempSt ls ts ats) = TempSt (tail ls) ts ats

nextTemps :: TempSt -> TempSt
nextTemps (TempSt ls ts ats) = TempSt ls (tail ts) ats

type TempM = State TempSt

getTemp :: TempM Temp
getTemp = gets (head . tempSupply) <* modify nextTemps

newLabel :: TempM Label
newLabel = gets (head . labels) <* modify nextLabels

broadcastName :: Unique -> TempM Label
broadcastName (Unique i) = do
    l <- newLabel
    modifying atLabelsLens (IM.insert i l)
    pure l

lookupName :: Name a -> TempM Label
lookupName (Name _ (Unique i) _) =
    gets
        (IM.findWithDefault (error "Internal error in IR phase: could not look find label for name") i . atLabels)

push :: KempeTy () -> Exp -> Stmt
push = undefined

-- TODO figure out dip
data Stmt = Push (KempeTy ()) Exp
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
          | MovMem Exp Exp -- store e2 and address given by e1

data Exp = ConstInt Int64
         | ConstantPtr Int64
         | ConstBool Word8
         | Named Label
         | Reg Temp -- TODO: size?
         | Mem Exp
         | Do Stmt Exp
         | ExprIntBinOp IntBinOp Exp Exp
         | ExprIntRel RelBinOp Exp Exp

data RelBinOp = IntEqIR
              | IntNeqIR
              | IntLtIR
              | IntGtIR

data IntBinOp = IntPlusIR
              | IntTimesIR
              | IntDivIR
              | IntMinusIR
              | IntModIR
              | IntXorIR
              | IntShiftRIR
              | IntShiftLIR

writeModule :: Module () MonoStackType -> TempM [Stmt]
writeModule = foldMapA writeDecl

writeDecl :: KempeDecl () MonoStackType -> TempM [Stmt]
writeDecl (FunDecl _ (Name _ u _) _ _ as) = do
    bl <- broadcastName u
    (Labeled bl:) <$> writeAtoms as
writeDecl (ExtFnDecl ty (Name _ u _) _ _ cName) = do
    bl <- broadcastName u
    pure [Labeled bl, CCall ty cName]

writeAtoms :: [Atom MonoStackType] -> TempM [Stmt]
writeAtoms = foldMapA writeAtom

foldMapA :: (Applicative f, Traversable t, Monoid m) => (a -> f m) -> t a -> f m
foldMapA = (fmap fold .) . traverse

tyInt :: KempeTy ()
tyInt = TyBuiltin () TyInt

tyBool :: KempeTy ()
tyBool = TyBuiltin () TyBool

intOp :: IntBinOp -> TempM [Stmt]
intOp cons = do
    t0 <- getTemp
    t1 <- getTemp
    pure
        [ Pop tyInt t0
        , Pop tyInt t1
        , Push tyInt $ ExprIntBinOp cons (Reg t0) (Reg t1)
        ]

intRel :: RelBinOp -> TempM [Stmt]
intRel cons = do
    t0 <- getTemp
    t1 <- getTemp
    pure
        [ Pop tyInt t0 -- TODO: maybe plain mov is better/nicer than pop
        , Pop tyInt t1
        , Push tyBool $ ExprIntRel cons (Reg t0) (Reg t1)
        ]

-- need monad for fresh 'Temp's
-- | This throws exceptions on nonsensical input.
writeAtom :: Atom MonoStackType -> TempM [Stmt]
writeAtom (IntLit _ i)              = pure [Push tyInt (ConstInt $ fromInteger i)]
writeAtom (BoolLit _ b)             = pure [Push tyBool (ConstBool $ toByte b)]
writeAtom (AtName _ n)              = pure . KCall <$> lookupName n -- TODO: when to do tco?
writeAtom (AtBuiltin ([], _) Drop)  = error "Internal error: Ill-typed drop!"
writeAtom (AtBuiltin ([], _) Swap)  = error "Internal error: Ill-typed swap!"
writeAtom (AtBuiltin ([_], _) Swap) = error "Internal error: Ill-typed swap!"
writeAtom (AtBuiltin ([], _) Dup)   = error "Internal error: Ill-typed dup!"
writeAtom (AtBuiltin _ IntPlus)     = intOp IntPlusIR
writeAtom (AtBuiltin _ IntMinus)    = intOp IntMinusIR
writeAtom (AtBuiltin _ IntTimes)    = intOp IntTimesIR
writeAtom (AtBuiltin _ IntDiv)      = intOp IntDivIR -- what to do on failure?
writeAtom (AtBuiltin _ IntMod)      = intOp IntModIR
writeAtom (AtBuiltin _ IntXor)      = intOp IntXorIR
writeAtom (AtBuiltin _ IntShiftR)   = intOp IntShiftRIR
writeAtom (AtBuiltin _ IntShiftL)   = intOp IntShiftLIR
writeAtom (AtBuiltin _ IntEq)       = intRel IntEqIR

-- stack pointer

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
