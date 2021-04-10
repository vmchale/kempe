module Kempe.Asm.Arm.Keystone ( assembleArm
                              ) where

import qualified Data.ByteString    as BS
import           Kempe.Asm.Arm.Type
import           Keystone           (Architecture (ArchArm64), Mode (ModeBigEndian), assemble, open, runAssembler)
import qualified Keystone
import           Prettyprinter      (pretty)
import           System.IO.Unsafe   (unsafePerformIO)


assembleArm :: [Arm ArmReg a] -> Either Keystone.Error BS.ByteString
assembleArm instrs = unsafePerformIO $ runAssembler $ do
    ks <- open ArchArm64 [ModeBigEndian]
    (enc, _) <- assemble ks [ show (pretty i) | i <- instrs ] Nothing
    pure enc
