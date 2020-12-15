-- | Simple-minded basic blocks. In particular, this does not detect labels that
-- are only targeted by only one jump.
module Kempe.Asm.X86.BasicBlock ( BasicBlock (..)
                                , splitInstr
                                ) where

import           Data.List.Split    (split, whenElt)
import           Kempe.Asm.X86.Type

data BasicBlock reg a = BasicBlock { blockAnn :: a
                                   , instr    :: [X86 reg ()] -- TODO: always () empty ann?
                                   }

-- | Split x86 instructions into basic blocks
splitInstr :: [X86 reg ()] -> [BasicBlock reg ()]
splitInstr = fmap (BasicBlock ()) . split (whenElt isCf) where
    isCf Jump{}    = True
    isCf Label{}   = True
    isCf Call{}    = True
    isCf CallBS{}  = True
    isCf BSLabel{} = True
    isCf Ret{}     = True
    isCf Jle{}     = True
    isCf Jg{}      = True
    isCf Jge{}     = True
    isCf Jne{}     = True
    isCf Jl{}      = True
    isCf Je{}      = True
    isCf _         = False
