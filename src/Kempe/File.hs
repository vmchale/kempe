module Kempe.File ( tcFile
                  , ppFile
                  , dumpTyAnn
                  , irFile
                  , x86File
                  , compile
                  ) where

-- common b/w test suite and exec, repl utils
import           Control.Composition       ((.*))
import           Control.Exception         (Exception, throwIO)
import qualified Data.ByteString.Lazy      as BSL
import           Kempe.AST
import           Kempe.Asm.X86.Type
import           Kempe.Error
import           Kempe.IR
import           Kempe.Parser
import           Kempe.Pipeline
import           Kempe.Proc.Nasm
import           Kempe.TyAssign
import           Prettyprinter             (Doc, hardline)
import           Prettyprinter.Render.Text (putDoc)

tcFile :: FilePath -> IO (Either (Error ()) ())
tcFile fp = do
    contents <- BSL.readFile fp
    (maxU, m) <- yeetIO $ parseWithMax contents
    pure $ fst <$> runTypeM maxU (checkModule m)

yeetIO :: Exception e => Either e a -> IO a
yeetIO = either throwIO pure

ppFile :: FilePath -> IO ()
ppFile fp = do
    contents <- BSL.readFile fp
    (_, m) <- yeetIO $ parseWithMax contents
    putDoc $ prettyModule m

dumpTyAnn :: FilePath -> IO ()
dumpTyAnn fp = do
    contents <- BSL.readFile fp
    (i, m) <- yeetIO $ parseWithMax contents
    (res, _) <- yeetIO $ runTypeM i (assignModule m)
    putDoc $ prettyTypedModule res

dumpIR :: Int -> Module a b -> Doc ann
dumpIR = prettyIR . fst .* irGen

dumpX86 :: Int -> Module a b -> Doc ann
dumpX86 = prettyAsm .* x86Alloc

irFile :: FilePath -> IO ()
irFile fp = do
    contents <- BSL.readFile fp
    res <- yeetIO $ parseWithMax contents
    putDoc $ uncurry dumpIR res <> hardline

x86File :: FilePath -> IO ()
x86File fp = do
    contents <- BSL.readFile fp
    res <- yeetIO $ parseWithMax contents
    putDoc $ uncurry dumpX86 res <> hardline

compile :: FilePath -> FilePath -> IO ()
compile fp o = do
    contents <- BSL.readFile fp
    res <- yeetIO $ parseWithMax contents
    writeO (uncurry dumpX86 res) o
