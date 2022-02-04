{-# LANGUAGE QuantifiedConstraints, UndecidableInstances #-}

module Control.Monad.Trans.Compose where

import Control.Monad.Base
import Control.Monad.Error.Class
import Control.Monad.IO.Class
import Control.Monad.Reader.Class
import Control.Monad.RWS.Class (MonadRWS)
import Control.Monad.State.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Control
import Control.Monad.Trans.Elevator
import qualified Control.Monad.Trans.Except as T
import qualified Control.Monad.Trans.RWS.Lazy as LT
import qualified Control.Monad.Trans.RWS.Strict as ST
import qualified Control.Monad.Trans.Reader as T
import qualified Control.Monad.Trans.State.Lazy as LT
import qualified Control.Monad.Trans.State.Strict as ST
import qualified Control.Monad.Trans.Writer.Lazy as LT
import qualified Control.Monad.Trans.Writer.Strict as ST
import Control.Monad.Writer.Class
import Data.Kind

-- * 'ComposeT'
--
-- $composet
--
-- 'ComposeT' can be used in monad transformer stacks to derive instances.
--
-- This also allows the usage of these instances, while in the middle of the transformer stack.
-- This proves particularly useful, when writing a runner for a transformer stack.

-- | A newtype wrapper for two stacked monad transformers.
--
-- Access instances of the intermediate monad @(t2 m)@, whenever @t1@ implements 'MonadTrans' /
-- 'MonadTransControl'.
--
-- ==== Type level arguments
-- [@t1 :: ('Type' -> 'Type') -> 'Type' -> 'Type'@] outer monad transformer
-- [@t2 :: ('Type' -> 'Type') -> 'Type' -> 'Type'@] inner monad transformer
-- [@m :: 'Type' -> 'Type'@] monad
-- [@a :: 'Type'@] value
type ComposeT :: ((Type -> Type) -> Type -> Type) -- ^ @t1@
              -> ((Type -> Type) -> Type -> Type) -- ^ @t2@
              -> (Type -> Type) -- ^ @m@
              -> Type -- ^ @a@
              -> Type
newtype ComposeT t1 t2 m a = ComposeT { deComposeT :: t1 (t2 m) a }
  deriving newtype (Applicative, Functor, Monad)

instance (forall m. Monad m => Monad (t2 m), MonadTrans t1, MonadTrans t2) => MonadTrans (ComposeT t1 t2) where
  lift = ComposeT . lift . lift

instance (forall m. Monad m => Monad (t2 m), MonadTransControl t1, MonadTransControl t2) => MonadTransControl (ComposeT t1 t2) where
  type StT (ComposeT t1 t2) a = StT t2 (StT t1 a)
  liftWith f = defaultLiftWith2 ComposeT deComposeT $ \ x -> f x
  restoreT = defaultRestoreT2 ComposeT

-- | Elevated to @m@.
deriving via Elevator (ComposeT t1 t2) m
  instance
    ( Monad (t1 (t2 m))
    , MonadTrans (ComposeT t1 t2)
    , MonadIO m
    ) => MonadIO (ComposeT t1 t2 m)

-- | Elevated to @m@.
deriving via Elevator (ComposeT t1 t2) m
  instance
    ( Monad (t1 (t2 m))
    , MonadTrans (ComposeT t1 t2)
    , MonadBase b m
    ) => MonadBase b (ComposeT t1 t2 m)

-- | Elevated to @m@.
deriving via Elevator (ComposeT t1 t2) m
  instance
    ( Monad (t1 (t2 m))
    , MonadTransControl (ComposeT t1 t2)
    , MonadBaseControl b m
    ) => MonadBaseControl b (ComposeT t1 t2 m)

-- | /OVERLAPPABLE/.
-- Elevated to @(t2 m)@.
deriving via Elevator t1 (t2 (m :: * -> *))
  instance {-# OVERLAPPABLE #-}
    ( Monad (t1 (t2 m))
    , MonadTransControl t1
    , MonadError e (t2 m)
    ) => MonadError e (ComposeT t1 t2 m)

-- | Set by 'T.ExceptT'.
deriving via T.ExceptT e (t2 (m :: * -> *))
  instance
    ( Monad (t2 m)
    ) => MonadError e ((ComposeT (T.ExceptT e) t2) m)

-- | /OVERLAPPABLE/.
-- Elevated to @(t2 m)@.
deriving via Elevator t1 (t2 (m :: * -> *))
  instance {-# OVERLAPPABLE #-}
    ( Monad (t1 (t2 m))
    , MonadTransControl t1
    , MonadReader r (t2 m)
    ) => MonadReader r (ComposeT t1 t2 m)

-- | Set by 'T.ReaderT'.
deriving via T.ReaderT r (t2 (m :: * -> *))
  instance
    ( Monad (t2 m)
    ) => MonadReader r ((ComposeT (T.ReaderT r) t2) m)

-- | Set by 'LT.RWST'.
deriving via LT.RWST r w s (t2 (m :: * -> *))
  instance
    ( Monad (t2 m)
    , Monoid w
    ) => MonadReader r ((ComposeT (LT.RWST r w s) t2) m)

-- | Set by 'ST.RWST'.
deriving via ST.RWST r w s (t2 (m :: * -> *))
  instance
    ( Monad (t2 m)
    , Monoid w
    ) => MonadReader r ((ComposeT (ST.RWST r w s) t2) m)

-- | /OVERLAPPABLE/.
-- Elevated to @(t2 m)@.
deriving via Elevator t1 (t2 (m :: * -> *))
  instance {-# OVERLAPPABLE #-}
    ( Monad (t1 (t2 m))
    , MonadTransControl t1
    , MonadRWS r w s (t2 m)
    ) => MonadRWS r w s (ComposeT t1 t2 m)

-- | Set by 'LT.RWST'.
deriving via LT.RWST r w s (t2 (m :: * -> *))
  instance
    ( Monad (t2 m)
    , Monoid w
    ) => MonadRWS r w s ((ComposeT (LT.RWST r w s) t2) m)

-- | Set by 'ST.RWST'.
deriving via ST.RWST r w s (t2 (m :: * -> *))
  instance
    ( Monad (t2 m)
    , Monoid w
    ) => MonadRWS r w s ((ComposeT (ST.RWST r w s) t2) m)

-- | /OVERLAPPABLE/.
-- Elevated to @(t2 m)@.
deriving via Elevator t1 (t2 (m :: * -> *))
  instance {-# OVERLAPPABLE #-}
    ( Monad (t1 (t2 m))
    , MonadTrans t1
    , MonadState s (t2 m)
    ) => MonadState s (ComposeT t1 t2 m)

-- | Set by 'LT.StateT'.
deriving via LT.StateT s (t2 (m :: * -> *))
  instance
    ( Monad (t2 m)
    ) => MonadState s ((ComposeT (LT.StateT s) t2) m)

-- | Set by 'ST.StateT'.
deriving via ST.StateT s (t2 (m :: * -> *))
  instance
    ( Monad (t2 m)
    ) => MonadState s ((ComposeT (ST.StateT s) t2) m)

-- | Set by 'LT.RWST'.
deriving via LT.RWST r w s (t2 (m :: * -> *))
  instance
    ( Monad (t2 m)
    , Monoid w
    ) => MonadState s ((ComposeT (LT.RWST r w s) t2) m)

-- | Set by 'ST.RWST'.
deriving via ST.RWST r w s (t2 (m :: * -> *))
  instance
    ( Monad (t2 m)
    , Monoid w
    ) => MonadState s ((ComposeT (ST.RWST r w s) t2) m)

-- | /OVERLAPPABLE/.
-- Elevated to @(t2 m)@.
deriving via Elevator t1 (t2 (m :: * -> *))
  instance {-# OVERLAPPABLE #-}
    ( Monad (t1 (t2 m))
    , MonadTransControl t1
    , MonadWriter w (t2 m)
    ) => MonadWriter w (ComposeT t1 t2 m)

-- | Set by 'LT.WriterT'.
deriving via LT.WriterT w (t2 (m :: * -> *))
  instance
    ( Monad (t2 m)
    , Monoid w
    ) => MonadWriter w ((ComposeT (LT.WriterT w) t2) m)

-- | Set by 'ST.WriterT'.
deriving via ST.WriterT w (t2 (m :: * -> *))
  instance
    ( Monad (t2 m)
    , Monoid w
    ) => MonadWriter w ((ComposeT (ST.WriterT w) t2) m)

-- | Set by 'LT.RWST'.
deriving via LT.RWST r w s (t2 (m :: * -> *))
  instance
    ( Monad (t2 m)
    , Monoid w
    ) => MonadWriter w ((ComposeT (LT.RWST r w s) t2) m)

-- | Set by 'ST.RWST'.
deriving via ST.RWST r w s (t2 (m :: * -> *))
  instance
    ( Monad (t2 m)
    , Monoid w
    ) => MonadWriter w ((ComposeT (ST.RWST r w s) t2) m)


-- ** Run 'ComposeT'
--
-- $runComposet
--
-- You have to run the composed monad transformers to get back into the base monad at some point.

-- | Run a transformer stack.
--
-- This function takes the two individual monad transformer runners as arguments.
runComposeT :: (forall a. t1 (t2 m) a -> t2 m (StT t1 a)) -- ^ run @t1@
            -> (forall a. t2 m a -> m (StT t2 a)) -- ^ run @t2@
            -> (forall a. ComposeT t1 t2 m a -> m (StT t2 (StT t1 a)))
runComposeT runT1 runT2 = runT2 . runT1 . deComposeT

-- | Equivalent to 'runComposeT', but discards the monadic state 'StT'.
-- This is a simple approach when your monad transformer stack doesn't carry monadic state.
--
-- @
-- 'StT' ('ComposeT' t1 t2) a ~ a
-- @
--
-- This can be used to improve error messages when modifying a monad transformer stack.
runComposeT' :: (t1 (t2 m) a -> t2 m a) -- ^ run @t1@
             -> (t2 m a -> m a) -- ^ run @t2@
             -> (ComposeT t1 t2 m a -> m a)
runComposeT' runT1 runT2 = runT2 . runT1 . deComposeT

-- * Examples

-- ** Example 1: Create a new type class
--
-- $example1
--
-- When creating a new type class that supports 'ComposeT', you want to add recursive instances for
-- 'ComposeT'.
--
-- @
-- class 'Monad' m => MonadCustom m where
--   simpleMethod :: a -> m a
--   complicatedMethod :: (a -> m a) -> m a
-- @
--
-- You can easily derive those instances, after implementing an instance for 'Elevator'.
-- This is explained in "Control.Monad.Trans.Elevator".
--
-- Then it's possible to derive the recursive instance.
-- This is an /OVERLAPPABLE/ instance, because we want to be able to add new "base-case" instances
-- through transformers in a stack.
--
-- @
-- deriving via 'Elevator' t1 (t2 (m :: * -> *))
--   instance {-# OVERLAPPABLE #-}
--     ( 'Monad' (t1 (t2 m))
--     , 'MonadTransControl' t1
--     , MonadCustom (t2 m)
--     ) => MonadCustom ('ComposeT' t1 t2 m)
-- @

-- ** Example 2: Add an instance
--
-- $example2
--
-- Add a type class instance for a new monad transformer, when there already is a recursive instance
-- for 'ComposeT'.
--
-- @
-- newtype CustomT m a = CustomT { unCustomT :: 'Control.Monad.Trans.Identity.IdentityT' m a }
--   deriving newtype ('Functor', 'Applicative', 'Monad')
--   deriving newtype ('MonadTrans', 'MonadTransControl')
-- @
--
-- First we need the regular instance.
-- The method implementations are 'undefined' here, because they would only distract from
-- 'ComposeT'.
--
-- @
-- instance 'Monad' m => MonadCustom (CustomT m) where
--   simpleMethod = 'undefined'
--   complicatedMethod = 'undefined'
-- @
--
-- To add a "base-case" instance, that takes priority over the recursive instance,
-- /FlexibleInstances/ are required.
--
-- @
-- deriving via CustomT (t2 (m :: * -> *))
--   instance 'Monad' (t2 m) => MonadCustom (('ComposeT' CustomT t2) m)
-- @

-- ** Example 3: Build a transformer stack
--
-- $example3
--
-- Create a monad transformer stack and wrap it using a newtype.
--
-- @
-- type (|.) = 'ComposeT'
-- type Stack = 'LT.StateT' 'Int' |. 'T.ReaderT' 'Char' |. CustomT |. 'T.ReaderT' 'Bool' |. 'Control.Monad.Trans.Identity.IdentityT'
-- newtype StackT m a = StackT { unStackT :: Stack m a }
--   deriving newtype ('Functor', 'Applicative', 'Monad')
-- @
--
-- We are adding 'Control.Monad.Trans.Identity.IdentityT' to the end of the stack, so that all the
-- other transformer instances end up in the stack.
-- Now we can simply derive just the instances, that we want.
--
-- @
--   deriving newtype ('MonadState' 'Int')
--   deriving newtype MonadCustom
-- @
--
-- We can even access instances, that would have been shadowed in a regular transformer stack.
--
-- @
--   deriving newtype ('MonadReader' 'Bool')
-- @

-- ** Example 4: Run a transformer stack
--
-- $example4
--
-- This is the part, that actually contains your application logic.
-- Because of the setup with 'ComposeT', we won't have to worry about 'lift'ing during the
-- initialization.
--
-- @
-- runStackT :: 'MonadBaseControl' 'IO' m
--           => StackT m a
--           -> m (StT StackT a)
-- runStackT stackTma =
--   runStateT' |.
--     runReaderT' |.
--       runCustomT |.
--         (\\ tma -> 'T.runReaderT' tma 'True') |.
--           'Control.Monad.Trans.Identity.runIdentityT' $ unStackT stackTma
--   where
--     runReaderT' :: 'MonadReader' 'Bool' m => 'T.ReaderT' 'Char' m a -> m a
--     runReaderT' tma = do
--       bool <- 'ask'
--       let char = if bool
--                     then \'Y\'
--                     else \'N\'
--       'T.runReaderT' tma char
--
--     runStateT' :: 'MonadReader' 'Char' m => 'LT.StateT' 'Int' m a -> m (a, 'Int')
--     runStateT' tma = do
--       char <- 'ask'
--       let num = 'fromEnum' char
--       'LT.runStateT' tma num
-- @
