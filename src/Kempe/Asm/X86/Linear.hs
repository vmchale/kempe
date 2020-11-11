-- | Linear scan register allocator
--
-- See: https://web.stanford.edu/class/archive/cs/cs143/cs143.1128/lectures/17/Slides17.pdf
module Kempe.Asm.X86.Linear ( X86Reg (..)
                            , allocReg
                            ) where

import           Control.Monad.State (State, evalState)
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
type FreeSt = (S.Set X86Reg, S.Set X86Reg)

-- | Mark all registers as free (at the beginning).
allFree :: FreeSt
allFree = (S.fromList [Rax .. Rdx], S.fromList [AH .. DL])

type FreeM = State FreeSt

runFreeM :: FreeM a -> a
runFreeM = flip evalState allFree

-- TODO: generate spill code
allocReg :: [X86 AbsReg Liveness] -> [X86 X86Reg ()]
allocReg = undefined
