module Prettyprinter.Ext ( (<#>)
                         ) where

import           Prettyprinter

infixr 6 <#>

(<#>) :: Doc a -> Doc a -> Doc a
(<#>) x y = x <> hardline <> y
