{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Monad.Trans.Compose.Stack where

import Control.Monad.Trans.Compose
import Control.Monad.Trans.Compose.Transparent
import Data.Kind

-- | A data kind, which represents a monad transformer stack.
--
-- This is basically a type-level list of monad transformers.
data Stack
  = NilT
  | Stack :||> ((Type -> Type) -> Type -> Type)

infixl 1 :||>

-- | An isomorphism between a 'Stack' and the corresponding monad transformer, which can be built
-- using 'ComposeT'.
type family StackT (ts :: Stack) = (t :: (Type -> Type) -> Type -> Type) | t -> ts where
  StackT 'NilT = TransparentT
  StackT (ts ':||> t) = ComposeT t (StackT ts)

-- | A data type representing the runner function of a monad transformer stack.
--
-- This is basically a heterogeneous list of monad transformer runners.
--
-- 'RunStackT' can only be used for monad transformer stacks without monadic state t'Control.Monad.Trans.Control.StT'.
data RunStackT :: Stack -> (Type -> Type) -> Type -> Type where
  RunNilT :: RunStackT 'NilT m a
  (:..>) :: RunStackT ts m a -> (t (StackT ts m) a -> StackT ts m a) -> RunStackT (ts ':||> t) m a

infixl 1 :..>

-- | Run a transformer stack.
--
-- This takes a 'RunStackT' as an argument containing the individual runners.
--
-- 'runStackT' can only be used for monad transformer stacks without monadic state t'Control.Monad.Trans.Control.StT'.
runStackT :: RunStackT ts m a -> StackT ts m a -> m a
runStackT RunNilT = runTransparentT
runStackT (runRemainingStackT :..> runNextT) = runStackT runRemainingStackT . runNextT . deComposeT
