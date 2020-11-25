{-# LANGUAGE OverloadedStrings #-}

-- | Linear scan register allocator
--
-- See: https://web.stanford.edu/class/archive/cs/cs143/cs143.1128/lectures/17/Slides17.pdf
module Kempe.Asm.X86.Linear ( X86Reg (..)
                            , allocRegs
                            ) where

import           Control.Monad.State.Strict (State, evalState, gets)
import           Data.Foldable              (traverse_)
import qualified Data.Map                   as M
import           Data.Maybe                 (fromMaybe)
import qualified Data.Set                   as S
import           Kempe.Asm.X86.Type
import           Lens.Micro                 (Lens')
import           Lens.Micro.Mtl             (modifying, (.=))

-- set of free registers we iterate over
data AllocSt = AllocSt { allocs :: M.Map AbsReg X86Reg -- ^ Already allocated registers
                       , free64 :: S.Set X86Reg -- TODO: IntSet here?
                       , free8  :: S.Set X86Reg
                       }

allocsLens :: Lens' AllocSt (M.Map AbsReg X86Reg)
allocsLens f s = fmap (\x -> s { allocs = x }) (f (allocs s))

free64Lens :: Lens' AllocSt (S.Set X86Reg)
free64Lens f s = fmap (\x -> s { free64 = x }) (f (free64 s))

free8Lens :: Lens' AllocSt (S.Set X86Reg)
free8Lens f s = fmap (\x -> s { free8 = x }) (f (free8 s))

-- | Mark all registers as free (at the beginning).
allFree :: AllocSt
allFree = AllocSt mempty allReg64 (S.fromList [R8b .. R15b])

allReg64 :: S.Set X86Reg
allReg64 = S.fromList [R8 .. R15]

type AllocM = State AllocSt

runAllocM :: AllocM a -> a
runAllocM = flip evalState allFree

-- | On X86, certain registers interfere/are dependent. Thus, if we are using
-- some 'X86Reg', we need to remove several from the set of free registers up to
-- that point.
assoc :: X86Reg -> S.Set X86Reg
assoc Rax  = S.fromList [AH, AL]
assoc Rdx  = S.fromList [DH, DL]
assoc R8   = S.singleton R8b
assoc R9   = S.singleton R9b
assoc R10  = S.singleton R10b
assoc R11  = S.singleton R11b
assoc R12  = S.singleton R12b
assoc R13  = S.singleton R13b
assoc R14  = S.singleton R14b
assoc R15  = S.singleton R15b
assoc AH   = S.singleton Rax
assoc AL   = S.singleton Rax
assoc DH   = S.singleton Rdx
assoc DL   = S.singleton Rdx
assoc R8b  = S.singleton R8
assoc R9b  = S.singleton R9
assoc R10b = S.singleton R10
assoc R11b = S.singleton R11
assoc R12b = S.singleton R12
assoc R13b = S.singleton R13
assoc R14b = S.singleton R14
assoc R15b = S.singleton R15
assoc Rcx  = S.fromList [CH, CL]
assoc CH   = S.singleton Rcx
assoc CL   = S.singleton Rcx

allocRegs :: [X86 AbsReg Liveness] -> [X86 X86Reg ()]
allocRegs = runAllocM . traverse allocReg

new :: Liveness -> S.Set AbsReg
new (Liveness i o) = o S.\\ i

done :: Liveness -> S.Set AbsReg
done (Liveness i o) = i S.\\ o

freeDone :: Liveness -> AllocM ()
freeDone l = traverse_ freeAbsReg absRs
    where absRs = done l

freeAbsReg :: AbsReg -> AllocM ()
freeAbsReg (AllocReg64 i) = freeAbsReg64 i
freeAbsReg (AllocReg8 i)  = freeAbsReg8 i
freeAbsReg _              = pure () -- maybe sketchy?

freeAbsReg8 :: Int -> AllocM ()
freeAbsReg8 i = do
    xR <- findReg absR
    modifying allocsLens (M.delete absR)
    modifying free8Lens (S.insert xR)
    modifying free64Lens (<> assoc xR)

    where absR = AllocReg8 i

freeAbsReg64 :: Int -> AllocM ()
freeAbsReg64 i = do
    xR <- findReg absR
    modifying allocsLens (M.delete absR)
    modifying free64Lens (S.insert xR)
    modifying free8Lens (<> assoc xR)

    where absR = AllocReg64 i

assignReg64 :: Int -> X86Reg -> AllocM ()
assignReg64 i xr =
    modifying allocsLens (M.insert (AllocReg64 i) xr)

assignReg8 :: Int -> X86Reg -> AllocM ()
assignReg8 i xr =
    modifying allocsLens (M.insert (AllocReg8 i) xr)

newReg64 :: AllocM X86Reg
newReg64 = do
    r64St <- gets free64
    let (res, newSt) = fromMaybe err $ S.minView r64St
        assocRes = assoc res
    -- register is no longer free
    free64Lens .= newSt
    modifying free8Lens (S.\\ assocRes)
    pure res

    where err = error "(internal error) No register available."

newReg8 :: AllocM X86Reg
newReg8 = do
    r8St <- gets free8
    let (res, newSt) = fromMaybe err $ S.minView r8St
        assocRes = assoc res
    -- register is no longer free
    free8Lens .= newSt
    modifying free64Lens (S.\\ assocRes)
    pure res

    where err = error "(internal error) No register available."

findReg :: AbsReg -> AllocM X86Reg
findReg absR = gets
    (M.findWithDefault (error "Internal error in register allocator: unfound register") absR . allocs)

useReg64 :: Liveness -> Int -> AllocM X86Reg
useReg64 l i =
    if absR `S.member` new l
        then do { res <- newReg64 ; assignReg64 i res ; pure res }
        else findReg absR
    where absR = AllocReg64 i

useReg8 :: Liveness -> Int -> AllocM X86Reg
useReg8 l i =
    if absR `S.member` new l
        then do { res <- newReg8 ; assignReg8 i res ; pure res }
        else findReg absR
    where absR = AllocReg8 i

useAddr :: Liveness -> Addr AbsReg -> AllocM (Addr X86Reg)
useAddr l (Reg r)               = Reg <$> useReg l r
useAddr l (AddrRCPlus r c)      = AddrRCPlus <$> useReg l r <*> pure c
useAddr l (AddrRCMinus r c)     = AddrRCMinus <$> useReg l r <*> pure c
useAddr l (AddrRRPlus r0 r1)    = AddrRRPlus <$> useReg l r0 <*> useReg l r1
useAddr l (AddrRRScale r0 r1 c) = AddrRRScale <$> useReg l r0 <*> useReg l r1 <*> pure c

useReg :: Liveness -> AbsReg -> AllocM X86Reg
useReg l (AllocReg64 i) = useReg64 l i
useReg l (AllocReg8 i)  = useReg8 l i
useReg _ DataPointer    = pure Rbx
useReg _ CArg1          = pure Rdi -- shouldn't clobber anything because it's just used in function wrapper to push onto the kempe stack
useReg _ CArg2          = pure Rsi
useReg _ CArg3          = pure Rdx
useReg _ CArg4          = pure Rcx
useReg _ CArg5          = pure R8
useReg _ CArg6          = pure R9
useReg _ ShiftExponent  = pure CL
useReg _ CRet           = pure Rax -- shouldn't clobber anything because this is used at end of function calls/wrappers anyway
useReg _ QuotRes        = pure Rax
useReg _ RemRes         = pure Rdx
-- TODO: ig we should have a sanity check here?

-- There's no spill code buuut that's probably not necessary since the whole
-- kempe model is basically to start with everything pre-spilled
allocReg :: X86 AbsReg Liveness -> AllocM (X86 X86Reg ())
allocReg (PushReg l r)                         = PushReg () <$> useReg l r <* freeDone l
allocReg Ret{}                                 = pure $ Ret ()
allocReg (Call _ l)                            = pure $ Call () l
allocReg (PushConst _ i)                       = pure $ PushConst () i
allocReg (Je _ l)                              = pure $ Je () l
allocReg (Jump _ l)                            = pure $ Jump () l
allocReg (Label _ l)                           = pure $ Label () l
allocReg (MovRCBool l r b)                     = (MovRCBool () <$> useReg l r <*> pure b) <* freeDone l
allocReg (CmpAddrReg l a r)                    = (CmpAddrReg () <$> useAddr l a <*> useReg l r) <* freeDone l
allocReg (CmpAddrBool l a b)                   = (CmpAddrBool () <$> useAddr l a <*> pure b) <* freeDone l
allocReg (AddRC _ DataPointer c)               = pure $ AddRC () Rbx c
allocReg (SubRC _ DataPointer c)               = pure $ SubRC () Rbx c
allocReg (MovRA l r0 (Reg r1))                 = (MovRA () <$> useReg l r0 <*> fmap Reg (useReg l r1)) <* freeDone l
allocReg (SubRR l r0 r1)                       = (SubRR () <$> useReg l r0 <*> useReg l r1) <* freeDone l
allocReg (MovAR l a r)                         = (MovAR () <$> useAddr l a <*> useReg l r) <* freeDone l
allocReg (MovAC _ (Reg DataPointer) i)         = pure $ MovAC () (Reg Rbx) i
allocReg (MovRR l r0 r1)                       = (MovRR () <$> useReg l r0 <*> useReg l r1) <* freeDone l
allocReg (MovRA l r a)                         = (MovRA () <$> useReg l r <*> useAddr l a) <* freeDone l
allocReg (CmpRegReg l r0 r1)                   = (CmpRegReg () <$> useReg l r0 <*> useReg l r1) <* freeDone l
allocReg (MovABool _ (Reg DataPointer) b)      = pure $ MovABool () (Reg Rbx) b
allocReg (BSLabel _ b)                         = pure $ BSLabel () b
allocReg (MovRC l r c)                         = (MovRC () <$> useReg l r <*> pure c) <* freeDone l
allocReg (PopMem _ (AddrRCPlus DataPointer c)) = pure $ PopMem () (AddrRCPlus Rbx c)
allocReg (AddAC _ (Reg DataPointer) c)         = pure $ AddAC () (Reg Rbx) c
allocReg (AddRC l r c)                         = (AddRC () <$> useReg l r <*> pure c) <* freeDone l
allocReg (SubRC l r c)                         = (SubRC () <$> useReg l r <*> pure c) <* freeDone l
allocReg (MovAC l a c)                         = (MovAC () <$> useAddr l a <*> pure c) <* freeDone l
allocReg (MovACi8 l a c)                       = (MovACi8 () <$> useAddr l a <*> pure c) <* freeDone l
allocReg (MovABool l a b)                      = (MovABool () <$> useAddr l a <*> pure b) <* freeDone l
allocReg (PopMem l a)                          = PopMem () <$> useAddr l a <* freeDone l
allocReg (AddAC l a c)                         = (AddAC () <$> useAddr l a <*> pure c) <* freeDone l
allocReg (PushMem l a)                         = PushMem () <$> useAddr l a <* freeDone l
allocReg (AddRR l r0 r1)                       = (AddRR () <$> useReg l r0 <*> useReg l r1) <* freeDone l
allocReg (MovRL l r bl)                        = (MovRL () <$> useReg l r <*> pure bl) <* freeDone l
allocReg (XorRR l r0 r1)                       = (XorRR () <$> useReg l r0 <*> useReg l r1) <* freeDone l
allocReg (ShiftLRR l r0 r1)                    = (ShiftLRR () <$> useReg l r0 <*> useReg l r1) <* freeDone l
allocReg (ShiftRRR l r0 r1)                    = (ShiftRRR () <$> useReg l r0 <*> useReg l r1) <* freeDone l
allocReg (ImulRR l r0 r1)                      = (ImulRR () <$> useReg l r0 <*> useReg l r1) <* freeDone l
allocReg (MovRWord l r w)                      = (MovRWord () <$> useReg l r <*> pure w) <* freeDone l
allocReg (IdivR l r)                           = (IdivR () <$> useReg l r) <* freeDone l
allocReg Cqo{}                                 = pure $ Cqo ()
allocReg (PopReg l r)                          = (PopReg () <$> useReg l r) <* freeDone l
allocReg (MovRCi8 l r c)                       = (MovRCi8 () <$> useReg l r <*> pure c) <* freeDone l
allocReg (Jl _ l)                              = pure $ Jl () l
allocReg (MovACTag l a t)                      = (MovACTag () <$> useAddr l a <*> pure t) <* freeDone l
