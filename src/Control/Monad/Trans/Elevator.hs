{-# LANGUAGE QuantifiedConstraints, UndecidableInstances, TupleSections #-}

module Control.Monad.Trans.Elevator where

import Control.Applicative
import Control.Monad
import Control.Monad.Base
import Control.Monad.Cont.Class
import Control.Monad.Error.Class
import Control.Monad.Fix
import Control.Monad.Reader.Class
import Control.Monad.RWS.Class (MonadRWS)
import Control.Monad.State.Class
import Control.Monad.Trans
import Control.Monad.Trans.Control
import Control.Monad.Trans.Control.Identity
import Control.Monad.Writer.Class
import Control.Monad.Zip
import Data.Kind

-- * 'Elevator'
--
-- $elevator
--
-- 'Elevator' can be used to lift instances through monad transformers as long as they implement
-- 'MonadTrans' and 'MonadTransControl' instances.
--
-- 'MonadTransControl' is only necessary when there is atleast one method with a monadic argument.

-- | A newtype wrapper for monad transformers.
--
-- Access instances of the inner monad @m@.
--
-- __Type level arguments:__
--
-- [@t :: ('Type' -> 'Type') -> 'Type' -> 'Type'@] monad transformer
-- [@m :: 'Type' -> 'Type'@] monad
-- [@a :: 'Type'@] value
type Elevator :: ((Type -> Type) -> Type -> Type) -- ^ @t@
              -> (Type -> Type) -- ^ @m@
              -> Type -- ^ @a@
              -> Type
newtype Elevator t m a = Ascend { descend :: t m a }
  deriving newtype (Applicative, Functor, Monad)
  deriving newtype (MonadTrans, MonadTransControl, MonadTransControlIdentity)

instance (Monad (t m), MonadTrans t, MonadBase b m) => MonadBase b (Elevator t m) where
  liftBase = lift . liftBase

instance (Monad (t m), MonadTransControl t, MonadBaseControl b m) => MonadBaseControl b (Elevator t m) where
  type StM (Elevator t m) a = StM m (StT t a)
  liftBaseWith f = liftWith $ \ runT -> liftBaseWith $ \ runInBase -> f $ runInBase . runT
  restoreM = restoreT . restoreM

instance (Monad (t m), MonadTransControlIdentity t, MonadBaseControlIdentity b m) => MonadBaseControlIdentity b (Elevator t m) where
  liftBaseWithIdentity = defaultLiftBaseWithIdentity

instance (Monad (t m), MonadTransControl t, Monad m, Alternative m) => Alternative (Elevator t m) where
  empty = lift empty
  (<|>) x y = (restoreT . pure =<<) $ liftWith $ \ runT -> runT x <|> runT y

instance (Monad (t m), MonadTrans t, MonadFail m) => MonadFail (Elevator t m) where
  fail = lift . fail

instance (Monad (t m), MonadTransControlIdentity t, MonadFix m) => MonadFix (Elevator t m) where
  mfix f = liftWithIdentity $ \ runT -> mfix $ \ x -> runT $ f x

instance (Monad (t m), MonadTrans t, MonadIO m) => MonadIO (Elevator t m) where
  liftIO = lift . liftIO

instance (Monad (t m), MonadTransControl t, MonadPlus m) => MonadPlus (Elevator t m)

instance (Monad (t m), MonadTransControlIdentity t, MonadZip m) => MonadZip (Elevator t m) where
  mzip x y = liftWithIdentity $ \ runT ->
    mzip (runT x) (runT y)

instance (Monad (t m), MonadTransControl t, MonadCont m) => MonadCont (Elevator t m) where
  callCC f = (restoreT . pure =<<) $ liftWith $ \ runT ->
    callCC $ \ c -> runT $ f $ \ a -> restoreT $ c =<< runT (pure a)

instance (Monad (t m), MonadTransControl t, MonadError e m) => MonadError e (Elevator t m) where
  throwError = lift . throwError
  catchError throwing catching = (restoreT . pure =<<) $ liftWith $ \ runT ->
    catchError (runT throwing) (runT . catching)

instance (Monad (t m), MonadTransControl t, MonadReader r m) => MonadReader r (Elevator t m) where
  ask = lift ask
  local f tma = (restoreT . pure =<<) $ liftWith $ \ runT ->
    local f $ runT tma

instance (Monad (t m), MonadTransControl t, MonadRWS r w s m) => MonadRWS r w s (Elevator t m)

instance (Monad (t m), MonadTrans t, MonadState s m) => MonadState s (Elevator t m) where
  get = lift get
  put = lift . put

instance (Monad (t m), MonadTransControl t, MonadWriter w m) => MonadWriter w (Elevator t m) where
  tell = lift . tell
  listen tma = liftWith (\ runT -> listen $ runT tma) >>= \ (sta, w) ->
    (, w) <$> restoreT (pure sta)
  pass tma = lift . pass . pure =<< tma

-- * Examples

-- ** Example 1: Recover submerged instances
--
-- $example1
--
-- Let's assume you want to define a monad transformer stack.
--
-- @
-- newtype StackT m a = StackT { unStackT :: 'Control.Monad.Trans.Reader.ReaderT' 'Char' ('Control.Monad.Trans.Reader.ReaderT' 'Bool' m) a }
--   deriving newtype ('Functor', 'Applicative', 'Monad')
-- @
--
-- Now you want to expose the inner @('MonadReader' 'Bool')@ instance with @(StackT m)@.
--
-- Normally it's shadowed by the @('MonadReader' 'Char')@ instance, but we can use 'Elevator' to
-- access the inner transformer.
--
-- @
--   deriving ('MonadReader' 'Bool') via 'Elevator' ('Control.Monad.Trans.Reader.ReaderT' 'Char') ('Control.Monad.Trans.Reader.ReaderT' 'Bool' m)
-- @

-- ** Example 2: Custom transformer without boilerplate
--
-- $example2
--
-- Let's assume you have defined a monad transformer.
--
-- @
-- newtype CustomT m a = CustomT { unCustomT :: 'Control.Monad.Trans.Identity.IdentityT' m a }
--   deriving newtype ('Functor', 'Applicative', 'Monad')
--   deriving newtype ('MonadTrans', 'MonadTransControl')
--
-- runCustomT :: CustomT m a -> m a
-- runCustomT = 'Control.Monad.Trans.Identity.runIdentityT' . unCustomT
-- @
--
-- Now you want to use this monad transformer in a transformer stack.
--
-- @
-- newtype StackT m a = StackT { unStackT :: CustomT ('Control.Monad.Trans.Reader.ReaderT' 'Bool' m) a }
--   deriving newtype ('Functor', 'Applicative', 'Monad')
-- @
--
-- Unfortunately we can't derive a @('Monad' m => 'MonadReader' 'Bool' (StackT m))@ instance with
-- /GeneralizedNewtypeDeriving/, without also adding the instance to @CustomT@.
--
-- To still derive this trivial instance we can use 'Elevator' with /DerivingVia/.
--
-- @
--   deriving ('MonadReader' 'Bool') via ('Elevator' CustomT ('Control.Monad.Trans.Reader.ReaderT' 'Bool' m))
-- @

-- ** Example 3: Adding an instance for 'Elevator'
--
-- $example3
--
-- Suppose you define a new type class.
--
-- @
-- class 'Monad' m => MonadCustom m where
--   simpleMethod :: a -> m a
--   complicatedMethod :: (a -> m b) -> m b
-- @
--
-- A simple way to allow a type class to be lifted through other monad transformers is by adding an
-- instance for 'Elevator'.
--
-- You have to be careful about monadic state 'StT', when defining such instances using
-- 'MonadTransControl'.
--
-- @
-- instance (MonadCustom m, 'MonadTransControl' t) => MonadCustom ('Elevator' t m) where
--   simpleMethod = 'lift' . simpleMethod
--   complicatedMethod f = ('restoreT' . 'pure' '=<<') $ 'liftWith' $ \\ runT ->
--     complicatedMethod $ runT . f
-- @
--
-- Some useful examples (or exercises) are the instances for
-- [mtl](https://hackage.haskell.org/package/mtl)'s type classes ('MonadError', 'MonadReader',
-- 'MonadState', 'MonadWriter').
