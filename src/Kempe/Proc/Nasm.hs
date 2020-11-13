module Kempe.Proc.Nasm ( writeO
                       ) where

import           Data.Functor              (void)
import           Prettyprinter             (Doc, defaultLayoutOptions, layoutPretty)
import           Prettyprinter.Render.Text (renderIO)
import           System.IO.Temp            (withSystemTempFile)
import           System.Process            (proc, readCreateProcess)

-- | Assemble using @nasm@, output in some file.
writeO :: Doc ann -> FilePath -> IO ()
writeO p fpO = withSystemTempFile "kmp.S" $ \fp h -> do
    renderIO h (layoutPretty defaultLayoutOptions p)
    void $ readCreateProcess (proc "nasm" [fp, "-f", "elf64", "-o", fpO]) ""
