module Kempe.Asm.Arm.Keystone ( assembleArm
                              ) where

import qualified Data.ByteString    as BS
import           Kempe.Asm.Arm.Type
import           Keystone           (Architecture (ArchArm), Mode (ModeBigEndian), assemble, open, runAssembler)
import qualified Keystone
import           Prettyprinter      (pretty)

assembleArm :: [Arm ArmReg a] -> IO (Either Keystone.Error BS.ByteString)
assembleArm instrs = runAssembler $ do
    ks <- open ArchArm [ModeBigEndian]
    (enc, _) <- assemble ks [ show (pretty i) | i <- instrs ] Nothing
    pure enc
