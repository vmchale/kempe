module Main (main) where

import           Control.Exception   (throwIO)
import qualified Data.Version        as V
import           Kempe.File
import           Options.Applicative
import qualified Paths_kempe         as P

data Command = TypeCheck !FilePath
             | Compile !FilePath !FilePath !Bool !Bool -- TODO: take arch on cli

run :: Command -> IO ()
run (TypeCheck fp)            = either throwIO pure =<< tcFile fp
run (Compile _ _ False False) = putStrLn "Compiler currently does nothing; try 'typecheck'"
run (Compile fp _ True False) = irFile fp
run (Compile fp _ False True) = x86File fp

kmpFile :: Parser FilePath
kmpFile = argument str
    (metavar "FILE"
    <> help "Source file"
    <> kmpCompletions)

irSwitch :: Parser Bool
irSwitch = switch
    (long "dump-ir"
    <> help "Write intermediate representation to stdout")

asmSwitch :: Parser Bool
asmSwitch = switch
    (long "dump-asm"
    <> help "Write assembly (intel syntax) to stdout")

exeFile :: Parser FilePath
exeFile = argument str
    (metavar "OUTPUT"
    <> help "File output")

kmpCompletions :: HasCompleter f => Mod f a
kmpCompletions = completer . bashCompleter $ "file -X '!*.kmp' -o plusdirs"

commandP :: Parser Command
commandP = hsubparser
    (command "typecheck" (info tcP (progDesc "Type-check module contents")))
    <|> compileP
    where
        tcP = TypeCheck <$> kmpFile
        compileP = Compile <$> kmpFile <*> exeFile <*> irSwitch <*> asmSwitch

wrapper :: ParserInfo Command
wrapper = info (helper <*> versionMod <*> commandP)
    (fullDesc
    <> progDesc "Kempe language compiler, x86_64 and Aarch64 backend"
    <> header "Kempe - a stack-based language")

versionMod :: Parser (a -> a)
versionMod = infoOption (V.showVersion P.version) (short 'V' <> long "version" <> help "Show version")

main :: IO ()
main = run =<< execParser wrapper
