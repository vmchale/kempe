module CDecl ( goldenCDecl
             ) where

import qualified Data.ByteString.Lazy      as BSL
import           Data.Text.Lazy.Encoding   (encodeUtf8)
import           Kempe.File
import           Language.C.AST
import           Prettyprinter             (Doc, LayoutOptions (..), PageWidth (..), layoutSmart)
import           Prettyprinter.Render.Text (renderLazy)
import           Test.Tasty                (TestTree)
import           Test.Tasty.Golden         (goldenVsString)

renderBSL :: Doc ann -> BSL.ByteString
renderBSL = encodeUtf8 . renderLazy . layoutSmart settings where
    settings = LayoutOptions $ AvailablePerLine 180 0.8

compileOutput :: FilePath
              -> IO BSL.ByteString
compileOutput = fmap (renderBSL . prettyHeaders) . cDeclFile

goldenCDecl :: FilePath -- ^ Kempe file
            -> FilePath -- ^ Golden header file
            -> TestTree
goldenCDecl kFp golden =
    goldenVsString kFp golden (compileOutput kFp)
