{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS -Wno-unticked-promoted-constructors #-}

module Control.Monad.Trans.Compose.Stack where

import Control.Monad.Trans.Compose
import Control.Monad.Trans.Compose.Transparent
import Data.Kind

-- | A data kind representing a monad transformer stack.
--
-- This is basically a type-level list of monad transformers.
data Stack where
  -- | an empty monad transformer stack
  NilT :: Stack
  -- | add a monad transformer to a stack
  (:.|>) :: Stack -- ^ remaining stack
         -> ((Type -> Type) -> Type -> Type) -- ^ next monad transformer
         -> Stack

infixl 1 :.|>

-- | An isomorphism between a 'Stack' and the corresponding monad transformer, which can be built
-- using 'ComposeT'.
type family StackT (ts :: Stack) = (t :: (Type -> Type) -> Type -> Type) | t -> ts where
  StackT NilT = TransparentT
  StackT (ts :.|> t) = ComposeT t (StackT ts)

-- | A data type representing the runner function of a monad transformer stack.
--
-- This is basically a heterogeneous list of monad transformer runners.
--
-- 'RunStackT' can only be used for monad transformer stacks without monadic state t'Control.Monad.Trans.Control.StT'.
data RunStackT :: Stack -> (Type -> Type) -> Type -> Type where
  -- | run an empty monad transformer stack
  RunNilT :: RunStackT NilT m a
  -- | run the next monad transformer on a stack
  (:..>) :: RunStackT ts m a -- ^ run remaining stack
         -> (t (StackT ts m) a -> StackT ts m a) -- ^ run next monad transformer
         -> RunStackT (ts :.|> t) m a

infixl 1 :..>

-- | Run a monad transformer stack.
--
-- This takes a 'RunStackT' as an argument containing the individual runners.
--
-- 'runStackT' can only be used for monad transformer stacks without monadic state t'Control.Monad.Trans.Control.StT'.
runStackT :: RunStackT ts m a -> StackT ts m a -> m a
runStackT RunNilT = runTransparentT
runStackT (runRemainingStackT :..> runNextT) = runStackT runRemainingStackT . runNextT . deComposeT
