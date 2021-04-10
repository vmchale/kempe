module Kempe.Asm.X86.Keystone ( assembleX86
                              ) where

import qualified Data.ByteString    as BS
import           Kempe.Asm.X86.Type
import           Keystone           (Architecture (ArchX86), Mode (Mode64), OptionType (OptSyntax), OptionValue (SyntaxNasm), assemble, open, option, runAssembler)
import qualified Keystone
import           Prettyprinter      (pretty)

assembleX86 :: [X86 X86Reg a] -> IO (Either Keystone.Error BS.ByteString)
assembleX86 instrs = runAssembler $ do
    ks <- open ArchX86 [Mode64]
    option ks OptSyntax SyntaxNasm
    (enc, _) <- assemble ks [ show (pretty i) | i <- instrs ] Nothing
    pure enc
