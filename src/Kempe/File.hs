module Kempe.File ( tcFile
                  , dumpMono
                  , dumpTyped
                  , irFile
                  , x86File
                  , dumpX86
                  , compile
                  , parsedFp
                  ) where

-- common b/w test suite and exec, repl utils
import           Control.Composition       ((.*))
import           Control.Exception         (Exception, throwIO)
import           Data.Bifunctor            (bimap)
import qualified Data.ByteString.Lazy      as BSL
import           Data.Functor              (void)
import qualified Data.Set                  as S
import           Data.Tuple.Extra          (fst3)
import           Kempe.AST
import           Kempe.Asm.X86.Type
import           Kempe.Check.Pattern
import           Kempe.Error
import           Kempe.IR
import           Kempe.Lexer
import           Kempe.Parser
import           Kempe.Pipeline
import           Kempe.Proc.Nasm
import           Kempe.Shuttle
import           Kempe.TyAssign
import           Prettyprinter             (Doc, hardline)
import           Prettyprinter.Render.Text (putDoc)

tcFile :: FilePath -> IO (Either (Error ()) ())
tcFile fp = do
    contents <- BSL.readFile fp
    (maxU, m) <- yeetIO $ parseWithMax contents
    pure $ do
        void $ runTypeM maxU (checkModule m)
        mErr $ checkModuleExhaustive (void <$> m)

yeetIO :: Exception e => Either e a -> IO a
yeetIO = either throwIO pure

dumpTyped :: FilePath -> IO ()
dumpTyped fp = do
    (i, m) <- parsedFp fp
    (mTyped, _) <- yeetIO $ runTypeM i (assignModule m)
    putDoc $ prettyTypedModule mTyped

dumpMono :: FilePath -> IO ()
dumpMono fp = do
    (i, m) <- parsedFp fp
    (mMono, _) <- yeetIO $ monomorphize i m
    putDoc $ prettyTypedModule (fmap (bimap fromMonoConsAnn fromMono) mMono)
    where fromMono (is, os) = StackType S.empty is os
          fromMonoConsAnn (ConsAnn _ _ ty) = fromMono ty

dumpIR :: Int -> Module a c b -> Doc ann
dumpIR = prettyIR . fst3 .* irGen

dumpX86 :: Int -> Module a c b -> Doc ann
dumpX86 = prettyAsm .* x86Alloc

irFile :: FilePath -> IO ()
irFile fp = do
    res <- parsedFp fp
    putDoc $ uncurry dumpIR res <> hardline

parsedFp :: FilePath -> IO (Int, Module AlexPosn AlexPosn AlexPosn)
parsedFp fp = do
    contents <- BSL.readFile fp
    yeetIO $ parseWithMax contents

x86File :: FilePath -> IO ()
x86File fp = do
    res <- parsedFp fp
    putDoc $ uncurry dumpX86 res <> hardline

compile :: FilePath
        -> FilePath
        -> Bool -- ^ Debug symbols?
        -> IO ()
compile fp o dbg = do
    contents <- BSL.readFile fp
    res <- yeetIO $ parseWithMax contents
    writeO (uncurry dumpX86 res) o dbg
