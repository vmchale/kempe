{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module Kempe.Asm.X86.Type ( X86 (..)
                          , Addr (..)
                          ) where

import           Control.DeepSeq (NFData)
import qualified Data.ByteString as BS
import           Data.Int        (Int64)
import           Data.Word       (Word8)
import           GHC.Generics    (Generic)

type Label = Word

data Addr reg = Reg reg
              | AddrRRPlus reg reg
              | AddrRCPlus reg Int64
              | AddrRCMinus reg Int64
              | AddrRRScale reg reg Int64
              deriving (Generic, NFData)

-- TODO: sanity-check pass to make sure no Reg8's are in e.g. MovRCBool

-- parametric in @reg@; we do register allocation second
data X86 a reg = PushReg a reg
             | PopReg a reg
             | PushMem a (Addr reg)
             | PopMem a (Addr reg)
             | PushConst a Int64
             | Jump a Label
             | Call a Label
             | Ret a
             -- intel-ish syntax; destination first
             | MovRA a reg (Addr reg)
             | MovAR a (Addr reg) reg
             | MovABool a (Addr reg) Word8
             | MovRR a reg reg -- for convenience
             | MovRC a reg Int64
             | MovRCBool a reg Word8
             | AddRR a reg reg
             | SubRR a reg reg
             | MulRR a reg reg
             | AddRC a reg Int64
             | SubRC a reg Int64
             | Label a Label
             | BSLabel a BS.ByteString
             | Je a Label
             | CmpAddrReg a (Addr reg) reg
             | CmpRegReg a reg reg -- for simplicity
             deriving (Generic, NFData)

