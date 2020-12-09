module Main (main) where

import           Control.Exception         (throw)
import           Criterion.Main
import           Data.Bifunctor            (Bifunctor, bimap)
import qualified Data.ByteString.Lazy      as BSL
import qualified Data.Text                 as T
import           Kempe.Asm.X86
import           Kempe.Asm.X86.ControlFlow
import           Kempe.Asm.X86.Linear
import           Kempe.Asm.X86.Liveness
import           Kempe.Check.Pattern
import           Kempe.File
import           Kempe.IR
import           Kempe.IR.Opt
import           Kempe.Inline
import           Kempe.Lexer
import           Kempe.Module
import           Kempe.Monomorphize
import           Kempe.Parser
import           Kempe.Pipeline
import           Kempe.Shuttle
import           Kempe.TyAssign
import           Prettyprinter             (defaultLayoutOptions, layoutPretty)
import           Prettyprinter.Render.Text (renderStrict)

bivoid :: Bifunctor p => p a b -> p () ()
bivoid = bimap (const ()) (const ())

main :: IO ()
main =
    defaultMain [ env (BSL.readFile "test/data/lex.kmp") $ \contents ->
                  bgroup "parser"
                      [ bench "lex"   $ nf lexKempe contents
                      , bench "parse" $ nf parse contents
                      ]
                , env forTyEnv $ \ ~(p, s, prel) ->
                    bgroup "type assignment"
                      [ bench "check (test/data/ty.kmp)" $ nf runCheck p
                      , bench "check (prelude/fn.kmp)" $ nf runCheck prel
                      , bench "assign (test/data/ty.kmp)" $ nf runAssign p
                      , bench "assign (prelude/fn.kmp)" $ nf runAssign prel
                      , bench "shuttle (test/data/ty.kmp)" $ nf (uncurry monomorphize) p
                      , bench "shuttle (examples/splitmix.kmp)" $ nf (uncurry monomorphize) s
                      , bench "closedModule" $ nf (runSpecialize =<<) (runAssign p)
                      , bench "closure" $ nf (\m -> closure (m, mkModuleMap m)) (bivoid <$> snd p)
                      ]
                , env eitherMod $ \ e ->
                    bgroup "Pattern match exhaustiveness checker"
                        [ bench "lib/either.kmp" $ nf checkModuleExhaustive e
                        ]
                  , env parsedInteresting $ \ ~(f, n) ->
                      bgroup "Inliner"
                        [ bench "examples/factorial.kmp" $ nf inline (snd f)
                        , bench "lib/numbertheory.kmp" $ nf inline (snd n)
                        ]
                  , env irEnv $ \ ~(s, f, n) ->
                      bgroup "IR"
                        [ bench "IR pipeline (examples/splitmix.kmp)" $ nf (fst . runIR) s -- IR benchmarks are a bit silly; I will use them to decide if I should use difference lists
                        , bench "IR pipeline (examples/factorial.kmp)" $ nf (fst . runIR) f
                        , bench "IR pipeline (lib/numbertheory.kmp)" $ nf (fst . runIR) n
                        ]
                  , env envIR $ \ ~(s, f) ->
                      bgroup "opt"
                        [ bench "IR optimization (examples/splitmix.kmp)" $ nf optimize s
                        , bench "IR optimization (examples/factorial.kmp)" $ nf optimize f
                        ]
                  , env irEnv $ \ ~(s, f, _) ->
                      bgroup "Instruction selection"
                        [ bench "X86 (examples/factorial.kmp)" $ nf genX86 f
                        , bench "X86 (examples/splitmix.kmp)" $ nf genX86 s
                        ]
                  , env x86Env $ \ ~(s, f) ->
                      bgroup "Control flow graph"
                        [ bench "X86 (examples/factorial.kmp)" $ nf mkControlFlow f
                        , bench "X86 (examples/splitmix.kmp)" $ nf mkControlFlow s
                        ]
                  , env cfEnv $ \ ~(s, f, n) ->
                      bgroup "Liveness analysis"
                        [ bench "X86 (examples/factorial.kmp)" $ nf reconstruct f
                        , bench "X86 (examples/splitmix.kmp)" $ nf reconstruct s
                        , bench "X86 (lib/numbertheory.kmp)" $ nf reconstruct n
                        ]
                  , env absX86 $ \ ~(s, f, n) ->
                      bgroup "Register allocation"
                        [ bench "X86/linear (examples/factorial.kmp)" $ nf allocRegs f
                        , bench "X86/linear (examples/splitmix.kmp)" $ nf allocRegs s
                        , bench "X86/linear (lib/numbertheory.kmp)" $ nf allocRegs n
                        ]
                  , bgroup "Pipeline"
                        [ bench "Validate (examples/factorial.kmp)" $ nfIO (tcFile "examples/factorial.kmp")
                        , bench "Validate (examples/splitmix.kmp)" $ nfIO (tcFile "examples/splitmix.kmp")
                        , bench "Validate (lib/numbertheory.kmp)" $ nfIO (tcFile "lib/numbertheory.kmp")
                        , bench "Generate assembly (examples/factorial.kmp)" $ nfIO (writeAsm "examples/factorial.kmp")
                        , bench "Generate assembly (examples/splitmix.kmp)" $ nfIO (writeAsm "examples/splitmix.kmp")
                        , bench "Generate assembly (lib/numbertheory.kmp)" $ nfIO (writeAsm "lib/numbertheory.kmp")
                        , bench "Object file (examples/factorial.kmp)" $ nfIO (compile "examples/factorial.kmp" "/tmp/factorial.o" False)
                        , bench "Object file (lib/numbertheory.kmp)" $ nfIO (compile "lib/numbertheory.kmp" "/tmp/numbertheory.o" False)
                        , bench "Object file (examples/splitmix.kmp)" $ nfIO (compile "examples/splitmix.kmp" "/tmp/splitmix.o" False)
                        ]
                ]
    where parsedM = parseProcess "test/data/ty.kmp"
          splitmix = parseProcess "examples/splitmix.kmp"
          fac = parseProcess "examples/factorial.kmp"
          num = parseProcess "lib/numbertheory.kmp"
          eitherMod = snd <$> parseProcess "lib/either.kmp"
          parsedInteresting = (,) <$> fac <*> num
          prelude = parseProcess "prelude/fn.kmp"
          forTyEnv = (,,) <$> parsedM <*> splitmix <*> prelude
          runCheck (maxU, m) = runTypeM maxU (checkModule m)
          runAssign (maxU, m) = runTypeM maxU (assignModule m)
          runSpecialize (m, i) = runMonoM i (closedModule m)
          splitmixMono = either throw id . uncurry monomorphize <$> splitmix
          facMono = either throw id . uncurry monomorphize <$> fac
          numMono = either throw id . uncurry monomorphize <$> num
          irEnv = (,,) <$> splitmixMono <*> facMono <*> numMono
          -- TODO: bench optimization
          runIR = runTempM . yrrucnu writeModule
          genIR = fst . runTempM . yrrucnu writeModule
          genX86 m = let (ir, u) = runIR m in irToX86 undefined u (optimize ir)
          facIR = genIR <$> facMono
          splitmixIR = genIR <$> splitmixMono
          envIR = (,) <$> splitmixIR <*> facIR
          facX86 = genX86 <$> facMono
          splitmixX86 = genX86 <$> splitmixMono
          x86Env = (,) <$> splitmixX86 <*> facX86
          numX86 = uncurry x86Parsed <$> num
          facX86Cf = mkControlFlow <$> facX86
          splitmixX86Cf = mkControlFlow <$> splitmixX86
          numX86Cf = mkControlFlow <$> numX86
          cfEnv = (,,) <$> splitmixX86Cf <*> facX86Cf <*> numX86Cf
          facAbsX86 = reconstruct <$> facX86Cf
          splitmixAbsX86 = reconstruct <$> splitmixX86Cf
          numAbsX86 = reconstruct <$> numX86Cf
          absX86 = (,,) <$> splitmixAbsX86 <*> facAbsX86 <*> numAbsX86
          -- not even gonna justify this
          yrrucnu f (y, x) = f x y

writeAsm :: FilePath
         -> IO T.Text
writeAsm fp = do
    res <- parseProcess fp
    pure $ renderText $ uncurry dumpX86 res
    where renderText = renderStrict . layoutPretty defaultLayoutOptions
