module Kempe.Proc.Armasm ( writeO
                         ) where

import           Data.Functor              (void)
import           Prettyprinter             (Doc, defaultLayoutOptions, layoutPretty)
import           Prettyprinter.Render.Text (renderIO)
import           System.IO                 (hFlush)
import           System.IO.Temp            (withSystemTempFile)
import           System.Process            (CreateProcess (..), StdStream (Inherit), proc, readCreateProcess)

-- | Assemble using @armasm@, output in some file.
writeO :: Doc ann
       -> FilePath
       -> Bool -- ^ Debug symbols?
       -> IO ()
writeO p fpO dbg = withSystemTempFile "kmp.S" $ \fp h -> do
    renderIO h (layoutPretty defaultLayoutOptions p)
    hFlush h
    let debugFlag = if dbg then ("-g":) else id
    void $ readCreateProcess ((proc "nasm" (debugFlag [fp, fpO])) { std_err = Inherit }) ""

