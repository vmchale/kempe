{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

-- | Linear scan register allocator
--
-- See: https://web.stanford.edu/class/archive/cs/cs143/cs143.1128/lectures/17/Slides17.pdf
module Kempe.Asm.X86.Linear ( X86Reg (..)
                            , allocRegs
                            ) where

import           Control.DeepSeq     (NFData)
import           Control.Monad.State (State, evalState, gets)
import           Data.Foldable       (traverse_)
import qualified Data.Map            as M
import           Data.Maybe          (fromMaybe)
import qualified Data.Set            as S
import           GHC.Generics        (Generic)
import           Kempe.Asm.X86.Type
import           Lens.Micro          (Lens')
import           Lens.Micro.Mtl      (modifying, (.=))

-- currently just has 64-bit and 8-bit registers
data X86Reg = Rax
            | Rbx
            | Rcx
            | Rdx
            | Rsp
            | Rbp
            | AH
            | AL
            | BH
            | BL
            | CH
            | CL
            | DH
            | DL
            deriving (Eq, Ord, Enum, Bounded, Generic, NFData)

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
allFree = AllocSt mempty allReg64 (S.fromList [AH .. DL])

allReg64 :: S.Set X86Reg
allReg64 = S.fromList [Rax .. Rdx]

type AllocM = State AllocSt

runAllocM :: AllocM a -> a
runAllocM = flip evalState allFree

-- | On X86, certain registers interfere/are dependent. Thus, if we are using
-- some 'X86Reg', we need to remove several from the set of free registers up to
-- that point.
assoc :: X86Reg -> S.Set X86Reg
assoc Rax = S.fromList [AH, AL]
assoc Rbx = S.fromList [BH, BL]
assoc Rcx = S.fromList [CH, CL]
assoc Rdx = S.fromList [DH, DL]
assoc AH  = S.singleton Rax
assoc AL  = S.singleton Rax

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

newReg64 :: AllocM X86Reg
newReg64 = do
    r64St <- gets free64
    let (res, newSt) = fromMaybe err $ S.minView r64St --
        assocRes = assoc res
    -- register is no longer free
    free64Lens .= newSt
    modifying free8Lens (S.\\ assocRes)
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

-- FIXME: generate spill code
allocReg :: X86 AbsReg Liveness -> AllocM (X86 X86Reg ())
allocReg (PushReg l (AllocReg64 i)) = PushReg () <$> useReg64 l i <* freeDone l
allocReg Ret{}                      = pure $ Ret ()
allocReg (Call _ l)                 = pure $ Call () l
allocReg (PushConst _ i)            = pure $ PushConst () i
allocReg (Je _ l)                   = pure $ Je () l
allocReg (Jump _ l)                 = pure $ Jump () l
