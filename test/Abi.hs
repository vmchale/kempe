-- | This is to ensure consistency in the ABI.
module Abi ( backendGolden
           ) where

import           Control.Composition       ((.*))
import qualified Data.Text.Lazy            as TL
import           Data.Text.Lazy.Encoding   (encodeUtf8)
import           Data.Typeable             (Typeable)
import           Kempe.AST
import           Kempe.File
import           Kempe.Module
import           Prettyprinter             (defaultLayoutOptions, layoutPretty)
import           Prettyprinter.Render.Text (renderLazy)
import           Test.Tasty
import           Test.Tasty.Golden         (goldenVsString)

backendGolden :: TestTree
backendGolden =
    testGroup "IR goldens"
        [ goldenIR "test/data/abi.kmp" "test/golden/abi.ir"
        , goldenIR "lib/gaussian.kmp" "test/golden/gaussian.ir"
        -- not for ABI, to test it imports the right thing (transitively)
        -- FIXME: pending test , goldenIR "test/data/diamond/a.kmp" "test/golden/a.ir"
        ]

dumpIRLazyText :: Typeable a => Int -> Declarations a c b -> TL.Text
dumpIRLazyText = renderLazy . layoutPretty defaultLayoutOptions .* dumpIR

goldenIR :: FilePath
         -> FilePath
         -> TestTree
goldenIR fp out =
    goldenVsString fp out $
        do
            res <- parseProcess fp
            pure $ encodeUtf8 $ uncurry dumpIRLazyText res
