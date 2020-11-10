{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor  #-}
{-# LANGUAGE DeriveGeneric  #-}

module Kempe.Asm.X86.Type ( X86 (..)
                          , Addr (..)
                          , AbsReg (..)
                          , Label
                          ) where

import           Control.DeepSeq (NFData)
import qualified Data.ByteString as BS
import           Data.Int        (Int64)
import           Data.Word       (Word8)
import           GHC.Generics    (Generic)

type Label = Word

data AbsReg = DataPointer
            | AllocReg64 !Int -- TODO: register by size
            | AllocReg8 !Int
            | CRet -- x0 on aarch64
            deriving (Eq, Ord, Generic, NFData)

data Addr reg = Reg reg
              | AddrRRPlus reg reg
              | AddrRCPlus reg Int64
              | AddrRCMinus reg Int64
              | AddrRRScale reg reg Int64
              deriving (Generic, NFData)

-- TODO: sanity-check pass to make sure no Reg8's are in e.g. MovRCBool

-- parametric in @reg@; we do register allocation second
data X86 reg a = PushReg { ann :: a, rSrc :: reg }
               | PopReg { ann :: a, rSrc :: reg }
               | PushMem { ann :: a, addr :: Addr reg }
               | PopMem { ann :: a, addr :: Addr reg }
               | PushConst { ann :: a, iSrc :: Int64 }
               | Jump { ann :: a, label :: Label }
               | Call { ann :: a, label :: Label }
               | Ret { ann :: a }
               -- intel-ish syntax; destination first
               | MovRA { ann :: a, rDest :: reg, addrSrc :: Addr reg }
               | MovAR { ann :: a, addrDest :: Addr reg, rSrc :: reg }
               | MovABool { ann :: a, addrDest :: Addr reg, boolSrc :: Word8 }
               | MovRR { ann :: a, rDest :: reg, rSrc :: reg } -- for convenience
               | MovRC { ann :: a, rDest :: reg, iSrc :: Int64 }
               | MovRCBool { ann :: a, rDest :: reg, boolSrc :: Word8 }
               | AddRR { ann :: a, rAdd1 :: reg, rAdd2 :: reg }
               | SubRR { ann :: a, rSub1 :: reg, rSub2 :: reg }
               | MulRR { ann :: a, rMul1 :: reg, rMul2 :: reg }
               | AddRC { ann :: a, rAdd1 :: reg, iAdd2 :: Int64 }
               | SubRC { ann :: a, rSub1 :: reg, iSub2 :: Int64 }
               | Label { ann :: a, label :: Label }
               | BSLabel { ann :: a, bsLabel :: BS.ByteString }
               | Je { ann :: a, jLabel :: Label }
               | CmpAddrReg { ann :: a, addrCmp :: Addr reg, rCmp :: reg }
               | CmpRegReg { ann :: a, rCmp :: reg, rCmp' :: reg } -- for simplicity
               deriving (Generic, NFData, Functor)

