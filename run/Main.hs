module Main (main) where

import           Control.Exception         (Exception, throwIO)
import           Control.Monad             ((<=<))
import qualified Data.ByteString.Lazy      as BSL
import           Data.Semigroup            ((<>))
import qualified Data.Version              as V
import           Kempe.AST
import           Kempe.File
import           Kempe.Lexer
import           Kempe.Parser
import           Options.Applicative
import qualified Paths_kempe               as P
import           Prettyprinter             (LayoutOptions (LayoutOptions), PageWidth (AvailablePerLine), hardline, layoutSmart)
import           Prettyprinter.Render.Text (renderIO)
import           System.Exit               (ExitCode (ExitFailure), exitWith)
import           System.IO                 (stdout)

data Command = TypeCheck !FilePath
             | Compile !FilePath !(Maybe FilePath) !Bool !Bool !Bool -- TODO: take arch on cli
             | Format !FilePath

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
run (TypeCheck fp)                        = either throwIO pure =<< tcFile fp
run (Compile _ Nothing _ False False)     = putStrLn "No output file specified!"
run (Compile fp (Just o) dbg False False) = compile fp o dbg
run (Compile fp Nothing False True False) = irFile fp
run (Compile fp Nothing False False True) = x86File fp
run (Format fp)                           = fmt fp
run _                                     = putStrLn "Invalid combination of CLI options. Try kc --help" *> exitWith (ExitFailure 1)

kmpFile :: Parser FilePath
kmpFile = argument str
    (metavar "FILE"
    <> help "Source file"
    <> kmpCompletions)

fmtP :: Parser Command
fmtP = Format <$> kmpFile

debugSwitch :: Parser Bool
debugSwitch = switch
    (long "debug"
    <> short 'g'
    <> help "Include debug symbols")

irSwitch :: Parser Bool
irSwitch = switch
    (long "dump-ir"
    <> help "Write intermediate representation to stdout")

asmSwitch :: Parser Bool
asmSwitch = switch
    (long "dump-asm"
    <> help "Write assembly (intel syntax) to stdout")

exeFile :: Parser (Maybe FilePath)
exeFile = optional $ argument str
    (metavar "OUTPUT"
    <> help "File output")

kmpCompletions :: HasCompleter f => Mod f a
kmpCompletions = completer . bashCompleter $ "file -X '!*.kmp' -o plusdirs"

commandP :: Parser Command
commandP = hsubparser
    (command "typecheck" (info tcP (progDesc "Type-check module contents")))
    <|> hsubparser (command "fmt" (info fmtP (progDesc "Pretty-print a Kempe file")) <> internal)
    <|> compileP
    where
        tcP = TypeCheck <$> kmpFile
        compileP = Compile <$> kmpFile <*> exeFile <*> debugSwitch <*> irSwitch <*> asmSwitch

wrapper :: ParserInfo Command
wrapper = info (helper <*> versionMod <*> commandP)
    (fullDesc
    <> progDesc "Kempe language compiler, x86_64 backend"
    <> header "Kempe - a stack-based language")

versionMod :: Parser (a -> a)
versionMod = infoOption (V.showVersion P.version) (short 'V' <> long "version" <> help "Show version")

main :: IO ()
main = run =<< execParser wrapper
