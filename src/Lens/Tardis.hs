module Lens.Tardis ( modifyingForwards
                   , (.=>)
                   ) where

import           Control.Monad.Tardis.Class (MonadTardis, modifyForwards)
import           Lens.Micro                 (ASetter, over, set)

infix 4 .=>

modifyingForwards :: MonadTardis bw fw m => ASetter fw fw a b -> (a -> b) -> m ()
modifyingForwards lens f = modifyForwards (over lens f)

-- | Like '.=' but for the tardis monad.
(.=>) :: MonadTardis bw fw m => ASetter fw fw a b -> b -> m ()
(.=>) lens val = modifyForwards (set lens val)
