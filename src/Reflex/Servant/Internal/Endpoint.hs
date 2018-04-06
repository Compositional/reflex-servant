{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Reflex.Servant.Internal.Endpoint where

import Control.Monad.Identity
import Data.Coerce
import Reflex
import Reflex.Servant.Internal.GenericClientM
import Servant.Client.Core
import Servant.Client.Core


class EndpointConfig ec where
  type EndpointOf ec i o :: *
  mkEndpoint :: ec -> (i -> GenericClientM o) -> EndpointOf ec i o

type ServantClientRunner cfg m = cfg -> forall a. GenericClientM a -> m (Either ServantError a)

------------------------------------------------------------------------

-- | A fully instantiated endpoint config.
--
-- This is much simpler to use and recommended for trying out reflex-servant.
--
-- For anything more serious, 'ConfiguredEndpointConfig' is to be
-- recommended, because of these limitations:
--
--   * No 'traverseEndpoint'
--   * No automatic lifting into monad transformers. You need to call 'Reflex.Servant.reflexClient' for every concrete @m@.
--
data InstantiatedEndpointConfig t m = InstantiatedEndpointConfig
  { instantiatedEndpointConfigRunner :: ServantClientRunner () (Performable m)
  }

instance PerformEvent t m => EndpointConfig (InstantiatedEndpointConfig t m) where
  type EndpointOf (InstantiatedEndpointConfig t m) i o = Event t i -> m (Event t (Either ServantError o))
  mkEndpoint (InstantiatedEndpointConfig r) ep ev = endpoint r (Endpoint (((),) <$> ep)) ev

------------------------------------------------------------------------

-- | An 'EndpointConfig' that provides extra configuration to its the runner.
-- Typically, @c@ contains a base url.
data ConfiguredEndpointConfig c = ConfiguredEndpointConfig c

-- | An endpoint to be run by an implementation of @servant-client-core@ using
-- configuration @c@.
newtype Endpoint c i o = Endpoint { runEndpoint :: i -> (c, GenericClientM o)
                                  }

instance EndpointConfig (ConfiguredEndpointConfig c) where
  type EndpointOf (ConfiguredEndpointConfig c) i o = Endpoint c i o
  mkEndpoint (ConfiguredEndpointConfig c) = Endpoint . ((c,).)

------------------------------------------------------------------------

endpoint
  :: forall t m ec i o. PerformEvent t m
  => ServantClientRunner ec (Performable m)
  -> Endpoint ec i o
  -> Event t i
  -> m (Event t (Either ServantError o))
endpoint runner endpnt requestE = (coerceEvent :: Event t (Identity a) -> Event t a) <$>
  traverseEndpoint runner endpnt (coerceEvent requestE)

-- | Sequential traversal of zero or more requests.
--
-- By choosing (a,) for @f@, you can tag your request with extra
-- information. Choosing 'Identity' is the same as 'endpoint'.
-- Choosing '[]' has the effect of performing possibly multiple
-- events in sequence.
--
-- The special case of zero elements is NOT handled differently, so
-- although the \'response\' will be quick, it seems likely that it
-- will occur in a subsequent frame.
traverseEndpoint
  :: ( PerformEvent t m
     , Traversable f
     )
  => ServantClientRunner ec (Performable m)
  -> Endpoint ec i o
  -> Event t (f i)
  -> m (Event t (f (Either ServantError o)))
traverseEndpoint runner endpnt requestE = do
  performEvent $ ffor requestE $ traverse (uncurry runner . runEndpoint endpnt)

-- TODO:
-- A variation of traverseEndpoint could make the response event immediate
-- when null. It seems that we need to map (const (error "")) though.
-- Don't forget to update the traverseEndpoint doc.

-- TODO:
-- Implement parallel traverseEndpoint. Probably needs an extra constraint
-- on Performable m
