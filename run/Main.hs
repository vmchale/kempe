module Main (main) where

import           Control.Exception         (Exception, throwIO)
import           Control.Monad             ((<=<))
import qualified Data.ByteString.Lazy      as BSL
import qualified Data.Text.Lazy.IO         as TLIO
import qualified Data.Version              as V
import           Kempe.AST
import           Kempe.File
import           Kempe.Lexer
import           Kempe.Parser
import           Language.C.AST
import           Options.Applicative
import qualified Paths_kempe               as P
import           Prettyprinter             (LayoutOptions (LayoutOptions), PageWidth (AvailablePerLine), hardline, layoutSmart)
import           Prettyprinter.Render.Text (putDoc, renderIO, renderLazy)
import           System.Exit               (ExitCode (ExitFailure), exitWith)
import           System.IO                 (stdout)
import           System.Info               (arch)

data Arch = Aarch64
          | X64

data Command = TypeCheck !FilePath
             | Compile !FilePath !(Maybe FilePath) !Arch !Bool !Bool !Bool
             | Format !FilePath
             | Lint !FilePath
             | CDecl !FilePath !(Maybe FilePath)

cdecl :: FilePath -> IO ()
cdecl = putDoc . (<> hardline) . prettyHeaders <=< cDeclFile

writeCDecl :: FilePath -> FilePath -> IO ()
writeCDecl fp o = do
    ds <- cDeclFile fp
    TLIO.writeFile o (renderLazy $ layoutSmart cSettings $ prettyHeaders ds)

fmt :: FilePath -> IO ()
fmt = renderIO stdout <=< fmap (render . (<> hardline) . prettyModule) . parsedFp
    where render = layoutSmart settings
          settings = LayoutOptions $ AvailablePerLine 80 0.5

parsedFp :: FilePath -> IO (Module AlexPosn AlexPosn AlexPosn)
parsedFp fp = do
     contents <- BSL.readFile fp
     yeetIO $ parse contents

yeetIO :: Exception e => Either e a -> IO a
yeetIO = either throwIO pure

run :: Command -> IO ()
run (TypeCheck fp)                                = either throwIO pure =<< tcFile fp
run (Lint fp)                                     = maybe (pure ()) throwIO =<< warnFile fp
run (Compile _ Nothing _ _ False False)           = putStrLn "No output file specified!"
run (Compile fp (Just o) X64 dbg False False)     = compile fp o dbg
run (Compile fp (Just o) Aarch64 dbg False False) = armCompile fp o dbg
run (Compile fp Nothing _ False True False)       = irFile fp
run (Compile fp Nothing X64 False False True)     = x86File fp
run (Compile fp Nothing Aarch64 False False True) = armFile fp
run (Format fp)                                   = fmt fp
run (CDecl fp Nothing)                            = cdecl fp
run (CDecl fp (Just o))                           = writeCDecl fp o
run _                                             = putStrLn "Invalid combination of CLI options. Try kc --help" *> exitWith (ExitFailure 1)

kmpFile :: Parser FilePath
kmpFile = argument str
    (metavar "FILE"
    <> help "Source file"
    <> kmpCompletions)

fmtP :: Parser Command
fmtP = Format <$> kmpFile

lintP :: Parser Command
lintP = Lint <$> kmpFile

cdeclP :: Parser Command
cdeclP = CDecl <$> kmpFile <*> outFile

debugSwitch :: Parser Bool
debugSwitch = switch
    (long "debug"
    <> short 'g'
    <> help "Include debug symbols")

archFlag :: Parser Arch
archFlag = fmap parseArch $ optional $ strOption
    (long "arch"
    <> metavar "ARCH"
    <> help "Target architecture (x64 or aarch64)"
    <> completer (listCompleter ["x64", "aarch64"]))
    where parseArch :: Maybe String -> Arch
          parseArch str' = case (str', arch) of
            (Nothing, "aarch64") -> Aarch64
            (Nothing, "x86_64")  -> X64
            (Just "aarch64", _)  -> Aarch64
            (Just "arm64", _)    -> Aarch64
            (Just "x64", _)      -> X64
            (Just "x86_64", _)   -> X64
            (Just "x86-64", _)   -> X64
            (Just "amd64", _)    -> X64
            _                    -> error "Failed to parse architecture! Try one of x64, aarch64"

irSwitch :: Parser Bool
irSwitch = switch
    (long "dump-ir"
    <> help "Write intermediate representation to stdout")

asmSwitch :: Parser Bool
asmSwitch = switch
    (long "dump-asm"
    <> help "Write assembly (intel syntax) to stdout")

outFile :: Parser (Maybe FilePath)
outFile = optional $ argument str
    (metavar "OUTPUT"
    <> help "File output")

kmpCompletions :: HasCompleter f => Mod f a
kmpCompletions = completer . bashCompleter $ "file -X '!*.kmp' -o plusdirs"

commandP :: Parser Command
commandP = hsubparser
    (command "typecheck" (info tcP (progDesc "Type-check module contents"))
    <> command "lint" (info lintP (progDesc "Lint a file"))
    <> command "cdecl" (info cdeclP (progDesc "Generate C headers for exported Kempe code")))
    <|> hsubparser (command "fmt" (info fmtP (progDesc "Pretty-print a Kempe file")) <> internal)
    <|> compileP
    where
        tcP = TypeCheck <$> kmpFile
        compileP = Compile <$> kmpFile <*> outFile <*> archFlag <*> debugSwitch <*> irSwitch <*> asmSwitch

wrapper :: ParserInfo Command
wrapper = info (helper <*> versionMod <*> commandP)
    (fullDesc
    <> progDesc "Kempe language compiler for X86_64 and Aarch64"
    <> header "Kempe - a stack-based language")

versionMod :: Parser (a -> a)
versionMod = infoOption (V.showVersion P.version) (short 'V' <> long "version" <> help "Show version")

main :: IO ()
main = run =<< execParser wrapper
