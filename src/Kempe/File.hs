module Kempe.File ( tcFile
                  ) where

-- common b/w test suite and exec
import           Control.Exception    (throwIO)
import qualified Data.ByteString.Lazy as BSL
import           Data.Foldable        (traverse_)
import           Kempe.Error
import           Kempe.Parser
import           Kempe.TyAssign

tcFile :: FilePath -> IO (Either (Error ()) ())
tcFile fp = do
    contents <- BSL.readFile fp
    (maxU, ds) <- yeetIO $ parseWithMax contents
    pure $ runTypeM maxU (traverse_ tyInsert ds)
    where yeetIO = either throwIO pure