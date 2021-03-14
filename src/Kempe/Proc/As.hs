module Kempe.Proc.As ( writeO
                     ) where

import           Data.Functor                (void)
import           Prettyprinter               (Doc, defaultLayoutOptions, layoutPretty)
import           Prettyprinter.Render.String (renderString)
import           System.Process              (CreateProcess (..), StdStream (Inherit), proc, readCreateProcess)

-- | Assemble using @armasm@, output in some file.
writeO :: Doc ann
       -> FilePath
       -> Bool -- ^ Debug symbols?
       -> IO ()
writeO p fpO dbg = do
    let inp = renderString (layoutPretty defaultLayoutOptions p)
        debugFlag = if dbg then ("-g":) else id
    void $ readCreateProcess ((proc "as" (debugFlag ["--", "-o", fpO])) { std_err = Inherit }) inp
