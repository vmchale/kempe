module Kempe.Proc.As ( writeO
                     ) where

import           Data.Functor   (void)
import           Prettyprinter  (Doc)
import           System.Process (CreateProcess (..), StdStream (Inherit), proc, readCreateProcess)

-- | Assemble using @nasm@, output in some file.
writeO :: Doc ann
       -> FilePath
       -> Bool -- ^ Debug symbols?
       -> IO ()
writeO p fpO dbg =
    let debugFlag = if dbg then ("-g":) else id in
    void $ readCreateProcess ((proc "as" (debugFlag ["-msyntax=intel", "--", "-o", fpO])) { std_err = Inherit }) (show p)
