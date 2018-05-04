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

    -- * Configuration basics
  , basicConfig
  , defaultConfig
  , Config(..)

    -- * Basic endpoint configuration
  , InstantiatedEndpointConfig(..)

    -- * General endpoint configuration
  , ConfiguredEndpointConfig(..)
  , ServantClientRunner
  , GenericClientM(..)
  , Endpoint(..)
  , endpoint
  , traverseEndpoint

    -- * Product type configuration
  , Tuple(..)
  , Fish(..)
  , InductiveFish(..)
  , InductivePair(..)

    -- * Configuration type classes
  , EndpointConfig
  , ProductConfig

  ) where

import Reflex.Servant.Internal
import Servant.Client.Core
import Reflex
import Control.Monad.Identity
import Data.Coerce

-- | A very basic configuration for concrete @m@ that does not support 'traverseEndpoint'.
basicConfig :: ServantClientRunner () IO -> Config (InstantiatedEndpointConfig t m) Tuple
basicConfig runner = Config (InstantiatedEndpointConfig runner) Tuple

-- | A default configuration that makes the @ec@ (endpoint config) argument available to the endpoint runner.
defaultConfig :: ec -> Config (ConfiguredEndpointConfig ec) InductivePair
defaultConfig ec = Config (ConfiguredEndpointConfig ec) InductivePair
