{-# LANGUAGE QuantifiedConstraints, UndecidableInstances #-}

module Control.Monad.Trans.Compose where

import Control.Monad.Base
import Control.Monad.Error.Class
import Control.Monad.IO.Class
import Control.Monad.Reader.Class
import Control.Monad.State.Class
import Control.Monad.Trans
import Control.Monad.Trans.Control
import Control.Monad.Trans.Elevator
import Control.Monad.Writer.Class
import Data.Kind

-- | A newtype wrapper for two stacked monad transformers.
--
-- Access instances of the intermediate monad @('t2' 'm')@, whenever 't1' implements
-- 'MonadTrans'/'MonadTransControl'.
--
-- ==== Type level arguments
-- [@'t1' :: ('Type' -> 'Type') -> 'Type' -> 'Type'@] outer monad transformer
-- [@'t2' :: ('Type' -> 'Type') -> 'Type' -> 'Type'@] inner monad transformer
-- [@'m' :: 'Type' -> 'Type'@] monad
-- [@'a' :: 'Type'@] value
type ComposeT :: ((Type -> Type) -> Type -> Type) -- ^ 't1'
              -> ((Type -> Type) -> Type -> Type) -- ^ 't2'
              -> (Type -> Type) -- ^ 'm'
              -> Type -- ^ 'a'
              -> Type
newtype ComposeT t1 t2 m a = ComposeT { deComposeT :: t1 (t2 m) a }
  deriving newtype (Applicative, Functor, Monad)

instance (forall m. Monad m => Monad (t2 m), MonadTrans t1, MonadTrans t2) => MonadTrans (ComposeT t1 t2) where
  lift = ComposeT . lift . lift

instance (forall m. Monad m => Monad (t2 m), MonadTransControl t1, MonadTransControl t2) => MonadTransControl (ComposeT t1 t2) where
  type StT (ComposeT t1 t2) a = StT t2 (StT t1 a)
  liftWith f = defaultLiftWith2 ComposeT deComposeT $ \ x -> f x
  restoreT = defaultRestoreT2 ComposeT

-- | Elevated to 'm'.
deriving via Elevator (ComposeT t1 t2) m
  instance
    ( Monad (t1 (t2 m))
    , MonadTrans (ComposeT t1 t2)
    , MonadIO m
    ) => MonadIO (ComposeT t1 t2 m)

-- | Elevated to 'm'.
deriving via Elevator (ComposeT t1 t2) m
  instance
    ( Monad (t1 (t2 m))
    , MonadTrans (ComposeT t1 t2)
    , MonadBase b m
    ) => MonadBase b (ComposeT t1 t2 m)

-- | Elevated to 'm'.
deriving via Elevator (ComposeT t1 t2) m
  instance
    ( Monad (t1 (t2 m))
    , MonadTransControl (ComposeT t1 t2)
    , MonadBaseControl b m
    ) => MonadBaseControl b (ComposeT t1 t2 m)

-- | Elevated to @('t2' 'm')@.
deriving via Elevator t1 (t2 (m :: * -> *))
  instance
    ( Monad (t1 (t2 m))
    , MonadTransControl t1
    , MonadError e (t2 m)
    ) => MonadError e (ComposeT t1 t2 m)

-- | Elevated to @('t2' 'm')@.
deriving via Elevator t1 (t2 (m :: * -> *))
  instance
    ( Monad (t1 (t2 m))
    , MonadTransControl t1
    , MonadReader r (t2 m)
    ) => MonadReader r (ComposeT t1 t2 m)

-- | Elevated to @('t2' 'm')@.
deriving via Elevator t1 (t2 (m :: * -> *))
  instance
    ( Monad (t1 (t2 m))
    , MonadTrans t1
    , MonadState s (t2 m)
    ) => MonadState s (ComposeT t1 t2 m)

-- | Elevated to @('t2' 'm')@.
deriving via Elevator t1 (t2 (m :: * -> *))
  instance
    ( Monad (t1 (t2 m))
    , MonadTransControl t1
    , MonadWriter w (t2 m)
    ) => MonadWriter w (ComposeT t1 t2 m)

-- | Run a transformer stack.
--
-- This function takes the two individual monad transformer runners as arguments.
runComposeT :: (forall a. t1 (t2 m) a -> t2 m (StT t1 a)) -- ^ run 't1'
            -> (forall a. t2 m a -> m (StT t2 a)) -- ^ run 't2'
            -> (forall a. ComposeT t1 t2 m a -> m (StT t2 (StT t1 a)))
runComposeT runT1 runT2 = runT2 . runT1 . deComposeT

-- | Equivalent to 'runComposeT', but discards the monadic state 'StT'.
-- This is a simple approach when your monad transformer stack doesn't carry monadic state.
--
-- @
-- 'StT' ('ComposeT' 't1' 't2') a ~ a
-- @
--
-- This can be used to improve error messages when modifying a monad transformer stack.
runComposeT' :: (t1 (t2 m) a -> t2 m a) -- ^ run 't1'
             -> (t2 m a -> m a) -- ^ run 't2'
             -> (ComposeT t1 t2 m a -> m a)
runComposeT' runT1 runT2 = runT2 . runT1 . deComposeT

-- * Examples
--
-- $examples
--
-- == Example 1: Create a new type class
--
-- When creating a new type class that supports 'ComposeT', you want to add recursive instances for
-- `ComposeT`.
--
-- @
-- class Monad m => MonadCustom m where
--   simpleMethod :: a -> m a
--   complicatedMethod :: (a -> m a) -> m a
-- @
--
-- You can easily derive those instances, after implementing an instance for 'Elevator'.
-- This is the hardest part, since you have to be careful about using 'MonadTransControl'
-- correctly.
--
-- @
-- instance ( Monad (t m)
--          , MonadTransControl t
--          , MonadCustom m
--          ) => MonadCustom (Elevator t m) where
--   simpleMethod = lift . simpleMethod
--   complicatedMethod f = liftWith $ \ runT ->
--     f . runT -- TODO: Use correct implementation
-- @
--
-- Now it's possible to derive the recursive instance.
--
-- @
-- deriving via Elevator t1 (t2 (m :: * -> *))
--   instance
--     ( Monad (t1 (t2 m))
--     , MonadTransControl t1
--     , MonadCustom (t2 m)
--     ) => MonadCustom (ComposeT t1 t2 m)
-- @
--
-- == Example 2: Add an instance
--
-- Add a type class instance for a new monad transformer, when there already is a recursive instance for 'ComposeT'.
--
-- @
-- newtype CustomT m a = CustomT { unCustomT :: IdentityT m a }
--   deriving newtype (Functor, Applicative, Monad)
--   deriving newtype (MonadTrans, MonadTransControl)
-- @
--
-- First we need the regular instance.
-- The method implementations are 'undefined' here, because they are not related to 'ComposeT'.
--
-- @
-- instance Monad m => MonadCustom (CustomT m) where
--   simpleMethod = undefined
--   complicatedMethod = undefined
-- @
--
-- To add an instance that takes priority over the recursive instance, we need an /OVERLAPPING/ instance.
--
-- @
-- deriving via CustomT (t2 (m :: * -> *))
--   instance {-# OVERLAPPING #-}
--     ( Monad (t2 m)
--     ) => MonadCustom ((CustomT |. t2) m)
-- @
--
-- == Example 3: Build a transformer stack
--
-- Create a monad transformer stack and wrap it using a newtype.
--
-- @
-- type (|.) = ComposeT
-- type Stack = StateT Int |. ReaderT Char |. CustomT |. ReaderT Bool |. IdentityT
-- newtype StackT m a = StackT { unStackT :: Stack m a }
--   deriving newtype (Functor, Applicative, Monad)
--   deriving newtype (MonadTrans, MonadTransControl)
--   deriving newtype (MonadBase, MonadBaseControl)
-- @
--
-- Now we can simply derive just the instances, that we want.
--
-- @
--   deriving newtype (MonadState Int)
--   deriving newtype (MonadCustom)
-- @
--
-- We can even use 'Elevator' to access instances, that have been shadowed in the stack.
--
-- @
--   deriving (MonadReader Bool) via ( (           StateT Int
--                                     |. Elevator (ReaderT Char)
--                                     |.          CustomT
--                                     |.          ReaderT Bool
--                                     |.          IdentityT
--                                     )
--                                     m
--                                   )
-- @
--
-- == Example 4: Run a transformer stack
--
-- This is the part, that actually contains your application logic.
-- Because of the setup with `ComposeT`, we won't have to worry about 'lift'ing during the
-- initialization.
--
-- @
-- runStackT :: MonadBaseControl IO m
--           => StackT m a
--           -> m (StT StackT a)
-- runStackT stackTma = do
--   let
--     runReaderT' :: MonadReader Bool m => ReaderT Char m a -> m a
--     runReaderT' tma = do
--       bool <- ask
--       let char = if bool
--                     then \'Y\'
--                     then \'N\'
--       runReaderT tma char
--
--     runStateT' :: MonadReader Char m => StateT Int m a -> m (a, Int)
--     runStateT' tma = do
--       char <- ask
--       let num = fromEnum char
--       runStateT tma num
--
--   runStateT' |. runReaderT' |. runCustomT |. (\\ tma -> runReaderT tma True) |. runIdentityT $ unStackT stackTma
-- @
