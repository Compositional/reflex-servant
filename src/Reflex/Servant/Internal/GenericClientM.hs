{-# LANGUAGE CPP #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE DeriveFunctor #-}
module Reflex.Servant.Internal.GenericClientM where

import Servant.Client.Core

-- | Like @servant-client@'s @ClientM@; a computation that uses an API
-- at a URL that must be provided from outside.
newtype GenericClientM a = GenericClientM { runGenericClientM :: forall m. RunClient m => m a }
  deriving (Functor)

-- We can't derive these because of the forall quantifier

instance Applicative GenericClientM where
  pure x = GenericClientM (pure x)
  (GenericClientM f) <*> (GenericClientM a) = GenericClientM (f <*> a)

instance Monad GenericClientM where
  (GenericClientM m) >>= f = GenericClientM (m >>= (runGenericClientM . f))

instance RunClient GenericClientM where
  runRequest r = GenericClientM (runRequest r)
  throwServantError e = GenericClientM (throwServantError e)
  catchServantError (GenericClientM m) f = GenericClientM (catchServantError m (runGenericClientM . f))
#if MIN_VERSION_servant_client_core(0,13,0)
  streamingRequest r = GenericClientM (streamingRequest r)
#endif
