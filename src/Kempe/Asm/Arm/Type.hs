module Kempe.Asm.Arm.Type ( Label
                          , ArmReg (..)
                          , Arm (..)
                          ) where

-- r19-r28 calle-saved
-- r0-r7 result registers

type Label = Word

data ArmReg = R0
            | R1
            | R2
            | R3
            | R4
            | R5
            | R6
            | R7
            | R8
            | R9
            | R10
            | R11
            | R12
            | R13
            | R14
            | R15
            | R16
            | R17
            | R18
            | R19
            | R20
            | R21
            | R22
            | R23
            | R24
            | R25
            | R26
            | R27
            | R28
            | R29
            | R30

-- see: https://developer.arm.com/documentation/dui0068/b/arm-instruction-reference
data Arm reg a = Branch { ann :: a, label :: Label } -- like "
               | AddRR { ann :: a, res :: reg, inp1 :: reg, inp2 :: reg }
               | SubRR { ann :: a, res :: reg, inp1 :: reg, inp2 :: reg }
               | MovRR { ann :: a, dest :: reg, src :: reg }
               | AndRR { ann :: a, dest :: reg, inp1 :: reg, inp2 :: reg }
