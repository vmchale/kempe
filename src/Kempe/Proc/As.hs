module Kempe.Proc.As ( writeO
                     ) where

import           Data.Functor                (void)
import           Prettyprinter               (Doc, laoutCompact)
import           Prettyprinter.Render.String (renderString)
import           System.Info                 (arch)
import           System.Process              (CreateProcess (..), StdStream (Inherit), proc, readCreateProcess)

-- | @as@ on Aarch64 systems, or @aarch64-linux-gnu-as@ when
-- cross-assembling/cross-compiling.
assembler :: String
assembler =
    case arch of
        "x86_64" -> "aarch64-linux-gnu-as"
        _        -> "as"

-- | Assemble using @as@, output in some file.
writeO :: Doc ann
       -> FilePath
       -> Bool -- ^ Debug symbols?
       -> IO ()
writeO p fpO dbg = do
    let inp = renderString (laoutCompact p)
        debugFlag = if dbg then ("-g":) else id
    void $ readCreateProcess ((proc assembler (debugFlag ["-o", fpO, "--"])) { std_err = Inherit }) inp
