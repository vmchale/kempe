module Main (main) where

import           Control.Exception         (throw, throwIO)
import           Criterion.Main
import qualified Data.ByteString.Lazy      as BSL
import           Data.Functor              (void)
import           Kempe.Asm.X86
import           Kempe.Asm.X86.ControlFlow
import           Kempe.Asm.X86.Linear
import           Kempe.Asm.X86.Liveness
import           Kempe.File
import           Kempe.IR
import           Kempe.Lexer
import           Kempe.Monomorphize
import           Kempe.Parser
import           Kempe.Shuttle
import           Kempe.TyAssign

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
                      , bench "closure" $ nf (\m -> closure (m, mkModuleMap m)) (void <$> snd p)
                      ]
                  , env irEnv $ \ ~(s, f) ->
                      bgroup "IR"
                        [ bench "IR pipeline (examples/splitmix.kmp)" $ nf (fst . runIR) s -- IR benchmarks are a bit silly; I will use them to decide if I should use difference lists
                        , bench "IR pipeline (examples/factorial.kmp)" $ nf (fst . runIR) f
                        ]
                  , env facMono $ \f ->
                      bgroup "Instruction selection"
                        [ bench "X86 (examples/factorial.kmp)" $ nf genX86 f
                        ]
                  , env facX86 $ \f ->
                      bgroup "Control flow graph"
                        [ bench "X86 (examples/factorial.kmp)" $ nf mkControlFlow f
                        ]
                  , env facX86Cf $ \f ->
                      bgroup "Liveness analysis"
                        [ bench "X86 (examples/factorial.kmp)" $ nf reconstruct f
                        ]
                  , env absX86 $ \f ->
                      bgroup "Register allocation"
                        [ bench "X86/linear (examples/factorial.kmp)" $ nf allocRegs f
                        ]
                  , bgroup "Pipeline"
                        [ bench "Validate (examples/factorial.kmp)" $ nfIO (tcFile "examples/factorial.kmp")
                        , bench "Object file (examples/factorial.kmp)" $ nfIO (compile "examples/factorial.kmp" "/tmp/factorial.o" False)
                        ]
                ]
    where parsedM = yeetIO . parseWithMax =<< BSL.readFile "test/data/ty.kmp"
          splitmix = yeetIO . parseWithMax =<< BSL.readFile "examples/splitmix.kmp"
          fac = yeetIO . parseWithMax =<< BSL.readFile "examples/factorial.kmp"
          prelude = yeetIO . parseWithMax =<< BSL.readFile "prelude/fn.kmp"
          forTyEnv = (,,) <$> parsedM <*> splitmix <*> prelude
          yeetIO = either throwIO pure
          runCheck (maxU, m) = runTypeM maxU (checkModule m)
          runAssign (maxU, m) = runTypeM maxU (assignModule m)
          runSpecialize (m, i) = runMonoM i (closedModule m)
          splitmixMono = either throw id . uncurry monomorphize <$> splitmix
          facMono = either throw id . uncurry monomorphize <$> fac
          irEnv = (,) <$> splitmixMono <*> facMono
          runIR = runTempM . writeModule
          genX86 m = let (ir, u) = runIR m in irToX86 u ir
          facX86 = genX86 <$> facMono
          facX86Cf = mkControlFlow <$> facX86
          absX86 = reconstruct <$> facX86Cf
          -- facIR = runIR <$> facMono
