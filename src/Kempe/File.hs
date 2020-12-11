module Kempe.File ( tcFile
                  , warnFile
                  , dumpMono
                  , dumpTyped
                  , irFile
                  , x86File
                  , dumpX86
                  , compile
                  , dumpIR
                  ) where

-- common b/w test suite and exec, repl utils
import           Control.Composition       ((.*))
import           Control.Exception         (Exception, throwIO)
import           Data.Bifunctor            (bimap)
import           Data.Functor              (void)
import           Data.Semigroup            ((<>))
import qualified Data.Set                  as S
import           Data.Tuple.Extra          (fst3)
import           Kempe.AST
import           Kempe.Asm.X86.Type
import           Kempe.Check.Pattern
import           Kempe.Check.TopLevel
import           Kempe.Error
import           Kempe.IR
import           Kempe.Lexer
import           Kempe.Module
import           Kempe.Pipeline
import           Kempe.Proc.Nasm
import           Kempe.Shuttle
import           Kempe.TyAssign
import           Prettyprinter             (Doc, hardline)
import           Prettyprinter.Render.Text (putDoc)

tcFile :: FilePath -> IO (Either (Error ()) ())
tcFile fp = do
    (maxU, m) <- parseProcess fp
    pure $ do
        void $ runTypeM maxU (checkModule m)
        mErr $ checkModuleExhaustive (void <$> m)

warnFile :: FilePath -> IO (Maybe (Warning (AlexPosn)))
warnFile fp = do
    (_, m) <- parseProcess fp
    pure $ topLevelCheck m

yeetIO :: Exception e => Either e a -> IO a
yeetIO = either throwIO pure

dumpTyped :: FilePath -> IO ()
dumpTyped fp = do
    (i, m) <- parseProcess fp
    (mTyped, _) <- yeetIO $ runTypeM i (assignModule m)
    putDoc $ prettyTypedModule mTyped

dumpMono :: FilePath -> IO ()
dumpMono fp = do
    (i, m) <- parseProcess fp
    (mMono, _) <- yeetIO $ monomorphize i m
    putDoc $ prettyTypedModule (fmap (bimap fromMonoConsAnn fromMono) mMono)
    where fromMono (is, os) = StackType S.empty is os
          fromMonoConsAnn (ConsAnn _ _ ty) = fromMono ty

dumpIR :: Int -> Declarations a c b -> Doc ann
dumpIR = prettyIR . fst3 .* irGen

dumpX86 :: Int -> Declarations a c b -> Doc ann
dumpX86 = prettyAsm .* x86Alloc

irFile :: FilePath -> IO ()
irFile fp = do
    res <- parseProcess fp
    putDoc $ uncurry dumpIR res <> hardline

x86File :: FilePath -> IO ()
x86File fp = do
    res <- parseProcess fp
    putDoc $ uncurry dumpX86 res <> hardline

compile :: FilePath
        -> FilePath
        -> Bool -- ^ Debug symbols?
        -> IO ()
compile fp o dbg = do
    res <- parseProcess fp
    writeO (uncurry dumpX86 res) o dbg
