module Kempe.Proc.Nasm ( writeO
                       ) where

import           Data.Functor              (void)
import qualified Data.Text.Lazy.IO         as TLIO
import           Prettyprinter             (Doc, defaultLayoutOptions, layoutPretty)
import           Prettyprinter.Render.Text (renderLazy)
import           System.IO                 (hFlush)
import           System.IO.Temp            (withSystemTempFile)
import           System.Process            (CreateProcess (..), StdStream (Inherit), proc, readCreateProcess)

-- | Assemble using @nasm@, output in some file.
writeO :: Doc ann
       -> FilePath
       -> Bool -- ^ Debug symbols?
       -> IO ()
writeO p fpO dbg = withSystemTempFile "kmp.S" $ \fp h -> do
    let txt = renderLazy $ layoutPretty defaultLayoutOptions p
    TLIO.hPutStr h txt
    hFlush h
    let debugFlag = if dbg then ("-g":) else id
    -- -O1 is signed byte optimization but no multi-passes
    void $ readCreateProcess ((proc "nasm" (debugFlag [fp, "-f", "elf64", "-O1", "-o", fpO])) { std_err = Inherit }) ""
