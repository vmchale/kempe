module Harness ( goldenOutput
               ) where

import qualified Data.ByteString.Lazy       as BSL
import qualified Data.ByteString.Lazy.Char8 as ASCII
import           Data.Functor               (void)
import           Kempe.File
import           System.FilePath            ((</>))
import           System.IO.Temp
import           System.Process             (CreateProcess (std_err), StdStream (Inherit), proc, readCreateProcess)
import           Test.Tasty
import           Test.Tasty.Golden          (goldenVsString)

-- | Assemble using @nasm@, output in some file.
runGcc :: [FilePath]
       -> FilePath
       -> IO ()
runGcc fps o =
    void $ readCreateProcess ((proc "cc" (fps ++ ["-o", o])) { std_err = Inherit }) ""

compileOutput :: FilePath
              -> FilePath
              -> IO BSL.ByteString
compileOutput fp harness =
    withSystemTempDirectory "kmp" $ \dir -> do
        let oFile = dir </> "kempe.o"
            exe = dir </> "kempe"
        compile fp oFile False
        runGcc [oFile, harness] exe
        readExe exe
    where readExe fp' = ASCII.pack <$> readCreateProcess ((proc fp' []) { std_err = Inherit }) ""

goldenOutput :: FilePath -- ^ Kempe file
             -> FilePath -- ^ C test harness
             -> FilePath -- ^ Golden file path
             -> TestTree
goldenOutput kFp cFp golden =
    goldenVsString kFp golden (compileOutput kFp cFp)
