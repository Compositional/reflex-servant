{-# LANGUAGE TupleSections #-}
{-# LANGUAGE Rank2Types #-}
module Reflex.Servant
  (
    -- * Deriving the endpoints
    reflexClient
  , HasReflexClient
  , ReflexClient
  , GenericClientM
  , Endpoint

    -- * Using the endpoints
  , endpoint
  , taggedEndpoint
  , ServantClientRunner

  ) where

import Reflex.Servant.Internal
import Servant.Client.Core
import Reflex

type ServantClientRunner m = forall a. GenericClientM a -> m (Either ServantError a)

endpoint
  :: PerformEvent t m
  => ServantClientRunner (Performable m)
  -> Endpoint i o
  -> Event t i
  -> m (Event t (Either ServantError o))
endpoint runner endpnt requestE = do
  performEvent $ ffor requestE $
    runner . runEndpoint endpnt

taggedEndpoint
  :: PerformEvent t m
  => ServantClientRunner (Performable m)
  -> Endpoint i o
  -> Event t (a, i)
  -> m (Event t (a, Either ServantError o))
taggedEndpoint runner endpnt requestE = do
  performEvent $ ffor requestE $ \(a, req) -> do
    (a,) <$> runner (runEndpoint endpnt req)
