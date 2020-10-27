module Main (main) where

import           Control.Exception    (throwIO)
import           Criterion.Main
import qualified Data.ByteString.Lazy as BSL
import           Data.Functor         (void)
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
                , env forTyEnv $ \ ~(p, s) ->
                    bgroup "type assignment"
                      [ bench "check" $ nf runCheck p
                      , bench "assign" $ nf runAssign p
                      , bench "closedModule" $ nf (runSpecialize =<<) (runAssign p)
                      , bench "closure" $ nf (\m -> closure (m, mkModuleMap m)) (void <$> snd p)
                      , bench "shuttle" $ nf (uncurry monomorphize) p
                      , bench "shuttle" $ nf (uncurry monomorphize) s
                      ]
                ]
    where parsedM = yeetIO . parseWithMax =<< BSL.readFile "test/data/ty.kmp"
          splitmix = yeetIO . parseWithMax =<< BSL.readFile "examples/splitmix.kmp"
          forTyEnv = (,) <$> parsedM <*> splitmix
          yeetIO = either throwIO pure
          runCheck (maxU, m) = runTypeM maxU (checkModule m)
          runAssign (maxU, m) = runTypeM maxU (assignModule m)
          runSpecialize (m, i) = runMonoM i (closedModule m)
