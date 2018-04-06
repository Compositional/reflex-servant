{-# LANGUAGE TupleSections #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
module Reflex.Servant
  (
    -- * Deriving the endpoints
    reflexClient
  , HasReflexClient
  , ReflexClient

    -- * Simple configuration
  , Config(..)
  , InstantiatedEndpointConfig(..)
  , ServantClientRunner
  , GenericClientM(..)

    -- * General configuration
  , ConfiguredEndpointConfig(..)
  , Endpoint(..)
  , endpoint
  , traverseEndpoint

  , Tuple(..)
  , Fish(..)
  , InductiveFish(..)
  , InductivePair(..)

    -- * Types
  , EndpointConfig
  , ProductConfig

  ) where

import Reflex.Servant.Internal
import Servant.Client.Core
import Reflex
import Control.Monad.Identity
import Data.Coerce
