module Harness ( goldenOutput
               , crossGolden
               ) where

import qualified Data.ByteString.Lazy       as BSL
import qualified Data.ByteString.Lazy.Char8 as ASCII
import           Data.Functor               (void)
import           Kempe.File
import           System.FilePath            ((</>))
import           System.IO.Temp
import           System.Info                (arch)
import           System.Process             (CreateProcess (env, std_err), StdStream (Inherit), proc, readCreateProcess)
import           Test.Tasty
import           Test.Tasty.Golden          (goldenVsString)
import           Test.Tasty.HUnit           (assertBool, testCase)

data CC = CC
        | ArmCC

instance Show CC where
    show CC    = "cc"
    show ArmCC = "aarch64-linux-gnu-gcc"

-- | Assemble using @nasm@, output in some file.
runGcc :: CC
       -> [FilePath]
       -> FilePath
       -> IO ()
runGcc cc fps o =
    void $ readCreateProcess ((proc (show cc) (fps ++ ["-o", o])) { std_err = Inherit }) ""

compileOutput :: FilePath
              -> FilePath
              -> IO BSL.ByteString
compileOutput fp harness =
    withSystemTempDirectory "kmp" $ \dir -> do
        let oFile = dir </> "kempe.o"
            exe = dir </> "kempe"
            compiler = case arch of
                "x86_64"  -> compile
                "aarch64" -> armCompile
                _         -> error "Internal error in test suite! Must run on either x86_64 or aarch64"
        compiler fp oFile False
        runGcc CC [oFile, harness] exe
        readExe exe
    where readExe fp' = ASCII.pack <$> readCreateProcess ((proc fp' []) { std_err = Inherit }) ""

crossCompileOutput :: FilePath
                   -> FilePath
                   -> IO BSL.ByteString
crossCompileOutput fp harness =
    withSystemTempDirectory "kmp" $ \dir -> do
        let oFile = dir </> "kempe.o"
            exe = dir </> "kempe"
        armCompile fp oFile False
        runGcc ArmCC [oFile, harness] exe
        readExe exe
    where readExe fp' = ASCII.pack <$> readCreateProcess ((proc "qemu-aarch64-static" [fp']) { std_err = Inherit, env = qemuEnv }) ""
          qemuEnv = Just [("QEMU_LD_PREFIX", "/usr/aarch64-linux-gnu/")]

goldenOutput :: FilePath -- ^ Kempe file
             -> FilePath -- ^ C test harness
             -> FilePath -- ^ Golden file path
             -> TestTree
goldenOutput kFp cFp golden =
    goldenVsString kFp golden (compileOutput kFp cFp)

crossGolden :: FilePath -- ^ Kempe file
            -> FilePath -- ^ C test harness
            -> FilePath -- ^ Golden file path
            -> TestTree
crossGolden kFp cFp golden =
    goldenVsString kFp golden (crossCompileOutput kFp cFp)
