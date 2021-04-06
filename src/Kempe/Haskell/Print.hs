{-# LANGUAGE OverloadedStrings #-}

module Kempe.Haskell.Print ( showStack
                           ) where

import           Data.Bits       (Bits, rotateL, (.|.))
import qualified Data.ByteString as BS
import           Data.Int        (Int64)
import           Data.List       (foldl')
import qualified Data.Text       as T
import           Data.Word       (Word64, Word8)
import           Kempe.AST
import           Kempe.AST.Size

showStack :: SizeEnv -> [KempeTy a] -> BS.ByteString -> T.Text
showStack _ [] _ = mempty
showStack env (ty:tys) bs =
    let (a, as) = BS.splitAt (fromIntegral $ size' env ty) bs
        in showAtom ty a <> showStack env tys as

-- | Parse
showAtom :: KempeTy a
         -> BS.ByteString -- ^ Has to be the correct size
         -> T.Text
showAtom (TyBuiltin _ TyBool) b = case BS.unpack b of { [0] -> "False" ; [1] -> "True" ; _ -> error "Invalid internal state!" }
showAtom (TyBuiltin _ TyWord) b = showText (mkWord64 (BS.unpack b) :: Word64)
showAtom (TyBuiltin _ TyInt) b  = showText (mkWord64 (BS.unpack b) :: Int64)

showText :: Show a => a -> T.Text
showText = T.pack . show

-- I guess this is big-endian
mkWord64 :: (Bits a, Num a) => [Word8] -> a
mkWord64 = snd . foldl' (\(exp', b) b' -> (exp'-8, b .|. fromIntegral b' `rotateL` exp')) (56, 0)
