module Kempe.Asm.KeystoneDebug ( disp
                               ) where

import qualified Data.ByteString as BS
import           Numeric         (showHex)

bsHex :: BS.ByteString -> String
bsHex = concatMap (flip showHex " ") . BS.unpack

disp :: Show a => Either a BS.ByteString -> IO ()
disp (Right bs) = putStrLn (bsHex bs)
disp (Left err) = error (show err)
