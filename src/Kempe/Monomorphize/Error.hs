{-# LANGUAGE FlexibleContexts #-}

-- TODO: move this to Kempe.Monomorphize
module Kempe.Monomorphize.Error ( tryMono
                                , tryMonoConsAnn
                                ) where

import           Control.Monad.Except (MonadError, throwError)
import qualified Data.Set             as S
import           Kempe.AST
import           Kempe.Error

tryMono :: MonadError (Error ()) m => StackType () -> m MonoStackType
tryMono (StackType _ is os) | S.null (freeVars (is ++ os)) = pure (is, os)
                            | otherwise = throwError $ MonoFailed ()

-- TODO: possible to get rid of this?
tryMonoConsAnn :: MonadError (Error ()) m => ConsAnn (StackType ()) -> m (ConsAnn MonoStackType)
tryMonoConsAnn = traverse tryMono
