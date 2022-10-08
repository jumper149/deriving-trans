module DerivingTrans.Internal where

import Control.Monad.Trans.Reader

newtype NoReaderT m a = MkNoReaderT {unNoReaderT :: ReaderT () m a}

runNoReaderT :: NoReaderT m a -> m a
runNoReaderT tma = runReaderT (unNoReaderT tma) ()
