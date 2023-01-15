{-# LANGUAGE CPP #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Monad.Trans.Compose where

import Control.Monad.Base
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Control
import Control.Monad.Trans.Control.Identity
import Control.Monad.Trans.Elevator
import Data.Kind

#if defined(VERSION_exceptions)
import Control.Monad.Catch
import qualified Control.Monad.Catch.Pure as T
#endif

#if defined(VERSION_mtl)
import Control.Monad.Accum
import Control.Monad.Accum.OrphanInstances ()
import Control.Monad.Cont.Class
import Control.Monad.Error.Class
import Control.Monad.Reader.Class
import Control.Monad.RWS.Class (MonadRWS)
import Control.Monad.Select
import Control.Monad.Select.OrphanInstances ()
import Control.Monad.State.Class
import qualified Control.Monad.Trans.Accum as T
import qualified Control.Monad.Trans.Cont as T
import qualified Control.Monad.Trans.Except as T
import qualified Control.Monad.Trans.RWS.Lazy as LT
import qualified Control.Monad.Trans.RWS.Strict as ST
import qualified Control.Monad.Trans.Reader as T
import qualified Control.Monad.Trans.Select as T
import qualified Control.Monad.Trans.State.Lazy as LT
import qualified Control.Monad.Trans.State.Strict as ST
import qualified Control.Monad.Trans.Writer.Lazy as LT
import qualified Control.Monad.Trans.Writer.Strict as ST
import Control.Monad.Writer.Class
#endif

#if defined(VERSION_primitive)
import Control.Monad.Primitive
#endif

#if defined(VERSION_unliftio_core)
import Control.Monad.IO.Unlift
import Control.Monad.Turn
#endif

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
-- Access instances of the intermediate monad @(t2 m)@, whenever @t1@ implements 'MonadTrans' \/
-- 'MonadTransControl' \/ 'MonadTransControlIdentity'.
--
-- __Type level arguments:__
--
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

instance (MonadTrans t1, MonadTrans t2) => MonadTrans (ComposeT t1 t2) where
  lift = ComposeT . lift . lift

instance (MonadTransControl t1, MonadTransControl t2) => MonadTransControl (ComposeT t1 t2) where
  type StT (ComposeT t1 t2) a = StT t2 (StT t1 a)
  liftWith f = defaultLiftWith2 ComposeT deComposeT $ \ x -> f x
  restoreT = defaultRestoreT2 ComposeT

instance (MonadTransControlIdentity t1, MonadTransControlIdentity t2) => MonadTransControlIdentity (ComposeT t1 t2) where
  liftWithIdentity inner = ComposeT $ liftWithIdentity $ \ runId1 ->
    liftWithIdentity $ \ runId2 -> inner $ runId2 . runId1 . deComposeT

-- | Elevated to @m@.
deriving via Elevator (ComposeT t1 t2) m
  instance
    ( MonadIO m
    , MonadTrans (ComposeT t1 t2)
    ) => MonadIO (ComposeT t1 t2 m)

-- | Elevated to @m@.
deriving via Elevator (ComposeT t1 t2) m
  instance
    ( MonadBase b m
    , MonadTrans (ComposeT t1 t2)
    ) => MonadBase b (ComposeT t1 t2 m)

-- | Elevated to @m@.
deriving via Elevator (ComposeT t1 t2) m
  instance
    ( MonadBaseControl b m
    , MonadTransControl (ComposeT t1 t2)
    ) => MonadBaseControl b (ComposeT t1 t2 m)

-- | Elevated to @m@.
deriving via Elevator (ComposeT t1 t2) m
  instance
    ( MonadBaseControlIdentity b m
    , MonadTransControlIdentity (ComposeT t1 t2)
    ) => MonadBaseControlIdentity b (ComposeT t1 t2 m)

#if defined(VERSION_exceptions)
-- | /OVERLAPPABLE/.
-- Elevated to @(t2 m)@.
deriving via Elevator t1 (t2 (m :: Type -> Type))
  instance {-# OVERLAPPABLE #-}
    ( MonadThrow (t2 m)
    , MonadTrans t1
    ) => MonadThrow (ComposeT t1 t2 m)

-- | Set by 'T.CatchT'.
deriving via T.CatchT (t2 (m :: Type -> Type))
  instance
    ( Monad (t2 m)
    ) => MonadThrow (ComposeT T.CatchT t2 m)

-- | /OVERLAPPABLE/.
-- Elevated to @(t2 m)@.
deriving via Elevator t1 (t2 (m :: Type -> Type))
  instance {-# OVERLAPPABLE #-}
    ( MonadCatch (t2 m)
    , MonadTransControl t1
    ) => MonadCatch (ComposeT t1 t2 m)

-- | Set by 'T.CatchT'.
deriving via T.CatchT (t2 (m :: Type -> Type))
  instance
    ( Monad (t2 m)
    ) => MonadCatch (ComposeT T.CatchT t2 m)
#endif

#if defined(VERSION_mtl)
-- | /OVERLAPPABLE/.
-- Elevated to @(t2 m)@.
deriving via Elevator t1 (t2 (m :: Type -> Type))
  instance {-# OVERLAPPABLE #-}
    ( MonadAccum w (t2 m)
    , MonadTrans t1
    ) => MonadAccum w (ComposeT t1 t2 m)

-- | Set by 'T.AccumT'.
deriving via T.AccumT w (t2 (m :: Type -> Type))
  instance
    ( Monoid w
    , Monad (t2 m)
    ) => MonadAccum w (ComposeT (T.AccumT w) t2 m)

-- | /OVERLAPPABLE/.
-- Elevated to @(t2 m)@.
deriving via Elevator t1 (t2 (m :: Type -> Type))
  instance {-# OVERLAPPABLE #-}
    ( MonadCont (t2 m)
    , MonadTransControl t1
    ) => MonadCont (ComposeT t1 t2 m)

-- | Set by 'T.ContT'.
deriving via T.ContT r (t2 (m :: Type -> Type))
  instance MonadCont (ComposeT (T.ContT r) t2 m)

-- | /OVERLAPPABLE/.
-- Elevated to @(t2 m)@.
deriving via Elevator t1 (t2 (m :: Type -> Type))
  instance {-# OVERLAPPABLE #-}
    ( MonadError e (t2 m)
    , MonadTransControl t1
    ) => MonadError e (ComposeT t1 t2 m)

-- | Set by 'T.ExceptT'.
deriving via T.ExceptT e (t2 (m :: Type -> Type))
  instance
    ( Monad (t2 m)
    ) => MonadError e (ComposeT (T.ExceptT e) t2 m)

-- | /OVERLAPPABLE/.
-- Elevated to @(t2 m)@.
deriving via Elevator t1 (t2 (m :: Type -> Type))
  instance {-# OVERLAPPABLE #-}
    ( MonadReader r (t2 m)
    , MonadTransControl t1
    ) => MonadReader r (ComposeT t1 t2 m)

-- | Set by 'T.ReaderT'.
deriving via T.ReaderT r (t2 (m :: Type -> Type))
  instance
    ( Monad (t2 m)
    ) => MonadReader r (ComposeT (T.ReaderT r) t2 m)

-- | Set by 'LT.RWST'.
deriving via LT.RWST r w s (t2 (m :: Type -> Type))
  instance
    ( Monoid w
    , Monad (t2 m)
    ) => MonadReader r (ComposeT (LT.RWST r w s) t2 m)

-- | Set by 'ST.RWST'.
deriving via ST.RWST r w s (t2 (m :: Type -> Type))
  instance
    ( Monoid w
    , Monad (t2 m)
    ) => MonadReader r (ComposeT (ST.RWST r w s) t2 m)

-- | /OVERLAPPABLE/.
-- Elevated to @(t2 m)@.
deriving via Elevator t1 (t2 (m :: Type -> Type))
  instance {-# OVERLAPPABLE #-}
    ( MonadRWS r w s (t2 m)
    , MonadTransControl t1
    ) => MonadRWS r w s (ComposeT t1 t2 m)

-- | Set by 'LT.RWST'.
deriving via LT.RWST r w s (t2 (m :: Type -> Type))
  instance
    ( Monoid w
    , Monad (t2 m)
    ) => MonadRWS r w s (ComposeT (LT.RWST r w s) t2 m)

-- | Set by 'ST.RWST'.
deriving via ST.RWST r w s (t2 (m :: Type -> Type))
  instance
    ( Monoid w
    , Monad (t2 m)
    ) => MonadRWS r w s (ComposeT (ST.RWST r w s) t2 m)

-- | /OVERLAPPABLE/.
-- Elevated to @(t2 m)@.
deriving via Elevator t1 (t2 (m :: Type -> Type))
  instance {-# OVERLAPPABLE #-}
    ( MonadSelect r (t2 m)
    , MonadTrans t1
    ) => MonadSelect r (ComposeT t1 t2 m)

-- | Set by 'T.SelectT'.
deriving via T.SelectT r (t2 (m :: Type -> Type))
  instance
    ( MonadTurn (t2 m)
    ) => MonadSelect r (ComposeT (T.SelectT r) t2 m)

-- | /OVERLAPPABLE/.
-- Elevated to @(t2 m)@.
deriving via Elevator t1 (t2 (m :: Type -> Type))
  instance {-# OVERLAPPABLE #-}
    ( MonadState s (t2 m)
    , MonadTrans t1
    ) => MonadState s (ComposeT t1 t2 m)

-- | Set by 'LT.StateT'.
deriving via LT.StateT s (t2 (m :: Type -> Type))
  instance
    ( Monad (t2 m)
    ) => MonadState s (ComposeT (LT.StateT s) t2 m)

-- | Set by 'ST.StateT'.
deriving via ST.StateT s (t2 (m :: Type -> Type))
  instance
    ( Monad (t2 m)
    ) => MonadState s (ComposeT (ST.StateT s) t2 m)

-- | Set by 'LT.RWST'.
deriving via LT.RWST r w s (t2 (m :: Type -> Type))
  instance
    ( Monoid w
    , Monad (t2 m)
    ) => MonadState s (ComposeT (LT.RWST r w s) t2 m)

-- | Set by 'ST.RWST'.
deriving via ST.RWST r w s (t2 (m :: Type -> Type))
  instance
    ( Monoid w
    , Monad (t2 m)
    ) => MonadState s (ComposeT (ST.RWST r w s) t2 m)

-- | /OVERLAPPABLE/.
-- Elevated to @(t2 m)@.
deriving via Elevator t1 (t2 (m :: Type -> Type))
  instance {-# OVERLAPPABLE #-}
    ( MonadWriter w (t2 m)
    , MonadTransControl t1
    ) => MonadWriter w (ComposeT t1 t2 m)

-- | Set by 'LT.WriterT'.
deriving via LT.WriterT w (t2 (m :: Type -> Type))
  instance
    ( Monoid w
    , Monad (t2 m)
    ) => MonadWriter w (ComposeT (LT.WriterT w) t2 m)

-- | Set by 'ST.WriterT'.
deriving via ST.WriterT w (t2 (m :: Type -> Type))
  instance
    ( Monoid w
    , Monad (t2 m)
    ) => MonadWriter w (ComposeT (ST.WriterT w) t2 m)

-- | Set by 'LT.RWST'.
deriving via LT.RWST r w s (t2 (m :: Type -> Type))
  instance
    ( Monoid w
    , Monad (t2 m)
    ) => MonadWriter w (ComposeT (LT.RWST r w s) t2 m)

-- | Set by 'ST.RWST'.
deriving via ST.RWST r w s (t2 (m :: Type -> Type))
  instance
    ( Monoid w
    , Monad (t2 m)
    ) => MonadWriter w (ComposeT (ST.RWST r w s) t2 m)
#endif

#if defined(VERSION_primitive)
-- | Elevated to @m@.
deriving via Elevator (ComposeT t1 t2) m
  instance
    ( PrimMonad m
    , MonadTrans (ComposeT t1 t2)
    ) =>
    PrimMonad (ComposeT t1 t2 m)
#endif

#if defined(VERSION_unliftio_core)
-- | Elevated to @m@.
deriving via Elevator (ComposeT t1 t2) m
  instance
    ( MonadUnliftIO m
    , MonadTransControlIdentity (ComposeT t1 t2)
    ) =>
    MonadUnliftIO (ComposeT t1 t2 m)
#endif


-- ** Run 'ComposeT'
--
-- $runComposet
--
-- You have to run the composed monad transformers to get back into the base monad at some point.

-- | Run two stacked monad transformers.
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
--   instance {-\# OVERLAPPABLE \#-}
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
-- newtype CustomT m a = CustomT { unCustomT :: t'Control.Monad.Trans.Identity.IdentityT' m a }
--   deriving newtype ('Functor', 'Applicative', 'Monad')
--   deriving newtype ('MonadTrans', 'MonadTransControl', 'MonadTransControlIdentity')
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
--   instance 'Monad' (t2 m) => MonadCustom ('ComposeT' CustomT t2 m)
-- @

-- ** Example 3: Build a transformer stack
--
-- $example3
--
-- Create a monad transformer stack and wrap it using a newtype.
--
-- @
-- type AppStackT = t'Control.Monad.Trans.Compose.Transparent.TransparentT' t'Control.Monad.Trans.Compose.Infix..|>' 'T.ReaderT' 'Bool' t'Control.Monad.Trans.Compose.Infix..|>' CustomT t'Control.Monad.Trans.Compose.Infix..|>' 'T.ReaderT' 'Char' t'Control.Monad.Trans.Compose.Infix..|>' 'LT.StateT' 'Int'
-- newtype AppT m a = AppT { unAppT :: AppStackT m a }
--   deriving newtype ('Functor', 'Applicative', 'Monad')
-- @
--
-- Using t'Control.Monad.Trans.Compose.Infix..|>' we can write @AppStackT@ in the order of initialization.
-- We are adding t'Control.Monad.Trans.Compose.Transparent.TransparentT' to the bottom of the stack,
-- so that all the other transformer instances actually end up in the stack.
-- Now we can simply derive just the instances, that we want.
--
-- @
--   deriving newtype ('MonadTrans', 'MonadTransControl')
--   deriving newtype ('Control.Monad.State.Class.MonadState' 'Int')
--   deriving newtype MonadCustom
-- @
--
-- We can even access instances, that would have been shadowed in a regular transformer stack.
--
-- @
--   deriving newtype ('Control.Monad.Reader.Class.MonadReader' 'Bool')
-- @

-- ** Example 4: Run a transformer stack
--
-- $example4
--
-- This is the part, that actually contains your application logic.
-- Because of the setup with 'ComposeT', we won't have to worry about 'lift'ing during the
-- initialization.
-- With 'Control.Monad.Trans.Compose.Infix...>' we can use the order of initialization again.
--
-- @
-- runAppT :: AppT m a -> m ('StT' AppT a)
-- runAppT appTma =
--   'Control.Monad.Trans.Compose.Transparent.runTransparentT'
--     'Control.Monad.Trans.Compose.Infix../>' (\\ tma -> 'T.runReaderT' tma 'True')
--     'Control.Monad.Trans.Compose.Infix../>' runCustomT
--     'Control.Monad.Trans.Compose.Infix../>' runReaderT'
--     'Control.Monad.Trans.Compose.Infix../>' runStateT'
--     $ unAppT appTma
--  where
--   runReaderT' :: 'Control.Monad.Reader.Class.MonadReader' 'Bool' m => 'T.ReaderT' 'Char' m a -> m a
--   runReaderT' tma = do
--     bool <- 'Control.Monad.Reader.Class.ask'
--     let char = if bool then \'Y\' else \'N\'
--     'T.runReaderT' tma char
--
--   runStateT' :: 'Control.Monad.Reader.Class.MonadReader' 'Char' m => 'LT.StateT' 'Int' m a -> m (a, 'Int')
--   runStateT' tma = do
--     char <- 'Control.Monad.Reader.Class.ask'
--     let num = 'fromEnum' char
--     'LT.runStateT' tma num
-- @
