{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Linear scan register allocator
module Kempe.Asm.Arm.Linear ( allocRegs
                            ) where

import           Control.Monad.State.Strict (State, evalState, gets)
import           Data.Foldable              (traverse_)
import qualified Data.IntMap                as IM
import qualified Data.IntSet                as IS
import           Data.Maybe                 (fromMaybe)
import           Data.Semigroup             ((<>))
import qualified Data.Set                   as S
import           Kempe.Asm.Arm.Type
import           Kempe.Asm.Type
import           Lens.Micro                 (Lens')
import           Lens.Micro.Mtl             (modifying, (.=))

data AllocSt = AllocSt { allocs :: IM.IntMap ArmReg -- ^ Already allocated registers
                       , free   :: S.Set ArmReg -- TODO: IntSet here?
                       }

allocsLens :: Lens' AllocSt (IM.IntMap ArmReg)
allocsLens f s = fmap (\x -> s { allocs = x }) (f (allocs s))

freeLens :: Lens' AllocSt (S.Set ArmReg)
freeLens f s = fmap (\x -> s { free = x }) (f (free s))

-- | Mark all registers as free (at the beginning).
allFree :: AllocSt
allFree = AllocSt mempty allReg

allReg :: S.Set ArmReg
allReg = S.fromList [X0 .. X29]

type AllocM = State AllocSt

runAllocM :: AllocM a -> a
runAllocM = flip evalState allFree

allocRegs :: [Arm AbsReg Liveness] -> [Arm ArmReg ()]
allocRegs = runAllocM . traverse allocReg

new :: Liveness -> IS.IntSet
new (Liveness i o) = o IS.\\ i

done :: Liveness -> IS.IntSet
done (Liveness i o) = i IS.\\ o

freeDone :: Liveness -> AllocM ()
freeDone l = traverse_ freeReg (IS.toList absRs)
    where absRs = done l

freeReg :: Int -> AllocM ()
freeReg i = do
    xR <- findReg i
    modifying allocsLens (IM.delete i)
    modifying freeLens (S.insert xR)

assignReg :: Int -> ArmReg -> AllocM ()
assignReg i xr =
    modifying allocsLens (IM.insert i xr)

newReg :: AllocM ArmReg
newReg = do
    rSt <- gets free
    let (res', newSt) = fromMaybe err $ S.minView rSt
    -- register is no longer free
    freeLens .= newSt
    pure res'

    where err = error "(internal error) No register available."

findReg :: Int -> AllocM ArmReg
findReg i = gets
    (IM.findWithDefault (error $ "Internal error in register allocator: unfound register" ++ show i) i . allocs)

useRegInt :: Liveness -> Int -> AllocM ArmReg
useRegInt l i =
    if i `IS.member` new l
        then do { res' <- newReg ; assignReg i res' ; pure res' }
        else findReg i

useAddr :: Liveness -> Addr AbsReg -> AllocM (Addr ArmReg)
useAddr l (Reg r)           = Reg <$> useReg l r
useAddr l (AddRCPlus r c)   = AddRCPlus <$> useReg l r <*> pure c
useAddr l (AddRRPlus r0 r1) = AddRRPlus <$> useReg l r0 <*> useReg l r1

useReg :: Liveness -> AbsReg -> AllocM ArmReg
useReg l (AllocReg i) = useRegInt l i
useReg _ DataPointer  = pure X19
useReg _ CArg0        = pure X0
useReg _ CArg1        = pure X1 -- shouldn't clobber anything because it's just used in function wrapper to push onto the kempe stack
useReg _ CArg2        = pure X2
useReg _ CArg3        = pure X3
useReg _ CArg4        = pure X4
useReg _ CArg5        = pure X5
useReg _ CArg6        = pure X6
useReg _ CArg7        = pure X7

allocReg :: Arm AbsReg Liveness -> AllocM (Arm ArmReg ())
allocReg Ret{}                      = pure $ Ret ()
allocReg (Branch _ l)               = pure $ Branch () l
allocReg (BranchLink _ l)           = pure $ BranchLink () l
allocReg (BranchCond _ l c)         = pure $ BranchCond () l c
allocReg (Label _ l)                = pure $ Label () l
allocReg (BSLabel _ l)              = pure $ BSLabel () l
allocReg (GnuMacro _ m)             = pure $ GnuMacro () m
allocReg (BranchZero l r lbl)       = (BranchZero () <$> useReg l r <*> pure lbl) <* freeDone l
allocReg (AddRR l r0 r1 r2)         = (AddRR () <$> useReg l r0 <*> useReg l r1 <*> useReg l r2) <* freeDone l
allocReg (SubRR l r0 r1 r2)         = (SubRR () <$> useReg l r0 <*> useReg l r1 <*> useReg l r2) <* freeDone l
allocReg (MulRR l r0 r1 r2)         = (MulRR () <$> useReg l r0 <*> useReg l r1 <*> useReg l r2) <* freeDone l
allocReg (SignedDivRR l r0 r1 r2)   = (SignedDivRR () <$> useReg l r0 <*> useReg l r1 <*> useReg l r2) <* freeDone l
allocReg (UnsignedDivRR l r0 r1 r2) = (UnsignedDivRR () <$> useReg l r0 <*> useReg l r1 <*> useReg l r2) <* freeDone l
allocReg (LShiftLRR l r0 r1 r2)     = (LShiftLRR () <$> useReg l r0 <*> useReg l r1 <*> useReg l r2) <* freeDone l
allocReg (LShiftRRR l r0 r1 r2)     = (LShiftRRR () <$> useReg l r0 <*> useReg l r1 <*> useReg l r2) <* freeDone l
allocReg (AndRR l r0 r1 r2)         = (AndRR () <$> useReg l r0 <*> useReg l r1 <*> useReg l r2) <* freeDone l
allocReg (AddRC l r0 r1 c)          = (AddRC () <$> useReg l r0 <*> useReg l r1 <*> pure c) <* freeDone l
allocReg (SubRC l r0 r1 c)          = (SubRC () <$> useReg l r0 <*> useReg l r1 <*> pure c) <* freeDone l
allocReg (MovRC l r0 c)             = (MovRC () <$> useReg l r0 <*> pure c) <* freeDone l
allocReg (MovRWord l r0 w)          = (MovRWord () <$> useReg l r0 <*> pure w) <* freeDone l
allocReg (Load l r a)               = (Load () <$> useReg l r <*> useAddr l a) <* freeDone l
allocReg (LoadLabel l r lbl)        = (LoadLabel () <$> useReg l r <*> pure lbl) <* freeDone l
allocReg (MovRR l r0 r1)            = (MovRR () <$> useReg l r0 <*> useReg l r1) <* freeDone l
allocReg (CSet l r c)               = (CSet () <$> useReg l r <*> pure c) <* freeDone l
allocReg (Store l r a)              = (Store () <$> useReg l r <*> useAddr l a) <* freeDone l
allocReg (StoreByte l r a)          = (StoreByte () <$> useReg l r <*> useAddr l a) <* freeDone l
allocReg (CmpRR l r0 r1)            = (CmpRR () <$> useReg l r0 <*> useReg l r1) <* freeDone l
allocReg (Neg l r0 r1)              = (Neg () <$> useReg l r0 <*> useReg l r1) <* freeDone l
allocReg (MulSubRRR l r0 r1 r2 r3)  = (MulSubRRR () <$> useReg l r0 <*> useReg l r1 <*> useReg l r2 <*> useReg l r3) <* freeDone l
allocReg (LoadByte l r a)           = (LoadByte () <$> useReg l r <*> useAddr l a) <* freeDone l
allocReg (XorRR l r0 r1 r2)         = (XorRR () <$> useReg l r0 <*> useReg l r1 <*> useReg l r2) <* freeDone l
allocReg (OrRR l r0 r1 r2)          = (OrRR () <$> useReg l r0 <*> useReg l r1 <*> useReg l r2) <* freeDone l
