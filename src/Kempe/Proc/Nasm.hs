module Kempe.Proc.Nasm ( writeO
                       ) where

import           Data.Functor              (void)
import           Prettyprinter             (Doc, defaultLayoutOptions, layoutPretty)
import           Prettyprinter.Render.Text (renderIO)
import           System.IO                 (hFlush)
import           System.IO.Temp            (withSystemTempFile)
import           System.Process            (CreateProcess (..), StdStream (Inherit), proc, readCreateProcess)

-- | Assemble using @nasm@, output in some file.
writeO :: Doc ann -> FilePath -> IO ()
writeO p fpO = withSystemTempFile "kmp.S" $ \fp h -> do
    renderIO h (layoutPretty defaultLayoutOptions p)
    hFlush h
    void $ readCreateProcess ((proc "nasm" [fp, "-f", "elf64", "-o", fpO]) { std_err = Inherit }) ""
