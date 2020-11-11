-- | Linear scan register allocator
--
-- See: https://web.stanford.edu/class/archive/cs/cs143/cs143.1128/lectures/17/Slides17.pdf
module Kempe.Asm.X86.Linear ( X86Reg (..)
                            , allocRegs
                            ) where

import           Control.Monad.State (State, evalState)
import qualified Data.Map            as M
import qualified Data.Set            as S
import           Kempe.Asm.X86.Type

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
            deriving (Eq, Ord, Enum, Bounded)

-- set of free registers we iterate over
data AllocSt = AllocSt { allocs :: M.Map AbsReg X86Reg
                       , free64 :: S.Set X86Reg
                       , free8  :: S.Set X86Reg
                       }

-- | Mark all registers as free (at the beginning).
allFree :: AllocSt
allFree = AllocSt mempty (S.fromList [Rax .. Rdx]) (S.fromList [AH .. DL])

type FreeM = State AllocSt

runFreeM :: FreeM a -> a
runFreeM = flip evalState allFree

-- deleteAssoc ::

allocRegs :: [X86 AbsReg Liveness] -> [X86 X86Reg ()]
allocRegs = runFreeM . traverse allocReg

-- FIXME: generate spill code
allocReg :: X86 AbsReg Liveness -> FreeM (X86 X86Reg ())
allocReg Ret{}           = pure $ Ret ()
allocReg (Call _ l)      = pure $ Call () l
allocReg (PushConst _ i) = pure $ PushConst () i
allocReg (Je _ l)        = pure $ Je () l
allocReg (Jump _ l)      = pure $ Jump () l
