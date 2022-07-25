module Control.Monad.Trans.Compose.Infix (
  type (.|>),
  (..>),
  (./>),
) where

import Control.Monad.Trans.Compose
import Control.Monad.Trans.Control

-- | A type-level infix operator for `ComposeT`.
type (.|>) = ComposeT

infixr 1 .|>

-- | An infix operator for `runComposeT`
(./>) :: (forall a. t1 (t2 m) a -> t2 m (StT t1 a)) -- ^ run @t1@
      -> (forall a. t2 m a -> m (StT t2 a)) -- ^ run @t2@
      -> (forall a. (t1 .|> t2) m a -> m (StT t2 (StT t1 a)))
(./>) = runComposeT

infixr 1 ./>

-- | An infix operator for `runComposeT'`
(..>) :: (t1 (t2 m) a -> t2 m a) -- ^ run @t1@
      -> (t2 m a -> m a) -- ^ run @t2@
      -> ((t1 .|> t2) m a -> m a)
(..>) = runComposeT'

infixr 1 ..>
