{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Reflex.Servant.Internal
  ( module Reflex.Servant.Internal
  , module Reflex.Servant.Internal.GenericClientM
  , module Reflex.Servant.Internal.Endpoint
  , module Reflex.Servant.Internal.Product
  ) where

import Data.Proxy
import GHC.TypeLits
import GHC.Exts(Constraint)
import Servant.API
#if MIN_VERSION_servant(0,13,0)
import Servant.API.Modifiers
#endif
import Servant.Client.Core
import Reflex.Servant.Internal.GenericClientM
import Reflex.Servant.Internal.TypeFunction
import Reflex.Servant.Internal.Endpoint
import Reflex.Servant.Internal.Product

import qualified Network.HTTP.Types                     as H

-- | A finished reflex client consisting of @:<|>@ and @Endpoint i o@
-- for various @i@ and @o@.
type ReflexClient config api = BuildReflexClient config I api

-- | Declares that reflex clients can be derived from this @api@.
--
-- This is 'HasReflexClient'' for complete API's. This indicates that @api@ is a self-sufficient API that works without being part of a larger API.
class HasReflexClient' config I api => HasReflexClient config api
instance HasReflexClient' config I api => HasReflexClient config api

------------------------------------------------------------------------

-- TODO: replace by something like servant-server's context?

-- | This composes a configuration from independent aspects like
-- endpoint style and which product type to use.
data Config e p = Config
  { -- | Should be an instance of 'EndpointConfig'
    --
    -- For example:
    --
    --  * 'InstantiatedEndpointConfig', to generate calls that look like @'Reflex.Class.Event' t i -> m ('Reflex.Class.Event' t (Either 'ServantError' o))@
    --  * 'ConfiguredEndpointConfig', to generate calls of a more general type that lets you instantiate @m@ at the call site and lets you use 'traverseEndpoint'
    configEndpoint :: e

    -- | Should be an instance of 'ProductConfig'
  , configProduct :: p
  }

-- | Pass-through
instance EndpointConfig ec => EndpointConfig (Config ec _p) where
  type EndpointOf (Config ec _p) i o = EndpointOf ec i o
  mkEndpoint c = mkEndpoint $ configEndpoint c

-- | Pass-through
instance ProductConfig p => ProductConfig (Config _e p) where
  type ProductOf (Config _e p) ts = ProductOf p ts
  type ProductConstraint (Config _e p) = ProductConstraint p
  productNil c = productNil $ configProduct c
  productCons c = productCons $ configProduct c
  productHead c = productHead $ configProduct c
  productTail c = productTail $ configProduct c

------------------------------------------------------------------------

-- | A configurable function from APIs to calls.
--
-- The @config@ parameter determines the details of call
-- generation. You should pass a 'Config' here; it helps you compose a
-- configuration.
reflexClient :: HasReflexClient config api => config -> Proxy api -> ReflexClient config api
reflexClient config api = reflexClient' config (Proxy @I) api

-- prefix:  single-endpoint api builder
-- api:     api (pattern matching)
-- | Declares that reflex clients can be derived from this @api@.
--
-- The @prefix@ type argument is a type level function (in the sense
-- of 'Reflex.Servant.Internal.TypeFunction.$$') that builds an API
-- that represents the relevant parts of the larger API that @api@ was
-- in. At the root of the API this is the identity
-- ('Reflex.Servant.Internal.TypeFunction.I') and it grows towards the
-- leaves.
--
-- The @api@ type argument is the subject of structural recursion using
-- pattern matching by instance head; it shrinks towards the leaves.
--
-- The @config@ type argument is just there as a \'global\' environment.
-- It gets passed around as a value too.
class HasReflexClient' (config :: *) (prefix :: *) (api :: *) where
  type BuildReflexClient config prefix api :: *
  reflexClient' :: config -> Proxy prefix -> Proxy api -> BuildReflexClient config prefix api

-- | Type level @Sub a = (a :>) = \b -> a :> b@
type Sub a = Lift ((:>) a)

class UncurryClient config (api :: *) where
  type Arguments config api :: [*]
  type Result config api :: *
  unCurry :: ProductConstraint config (Arguments config api) => config -> Proxy api -> Client GenericClientM api -> ProductOf config (Arguments config api) -> Result config api


-- Fish

instance ( HasReflexClient' cfg prefix a
         , HasReflexClient' cfg prefix b
         ) =>
         HasReflexClient' cfg prefix (a :<|> b) where
  type BuildReflexClient cfg prefix (a :<|> b) = BuildReflexClient cfg prefix a :<|> BuildReflexClient cfg prefix b

  reflexClient' cfg p _ = reflexClient' cfg p (Proxy @a) :<|> reflexClient' cfg p (Proxy @b)


-- Sub (aka "path" :> ...)

instance UncurryClient cfg more => UncurryClient cfg ((sym :: Symbol) :> more) where
  type Arguments  cfg (sym :> more) = Arguments cfg more
  type Result     cfg (sym :> more) = Result cfg more
  unCurry cfg _ c a = unCurry cfg (Proxy @more) c a

instance forall cfg prefix (sym :: Symbol) more.
         HasReflexClient' cfg (prefix :.: Sub sym) more =>
         HasReflexClient' cfg prefix (sym :> more) where
  type BuildReflexClient cfg prefix (sym :> more) =
       BuildReflexClient cfg (prefix :.: Sub sym) more
  reflexClient' cfg _ _ = reflexClient' cfg (Proxy @(prefix :.: Sub sym)) (Proxy @more)


-- Description

instance UncurryClient cfg more => UncurryClient cfg (Description sym :> more) where
  type Arguments  cfg (Description sym :> more) = Arguments cfg more
  type Result     cfg (Description sym :> more) = Result cfg more
  unCurry cfg _ c a = unCurry cfg (Proxy @more) c a



-- QueryParams

instance ( UncurryClient cfg more
         , ProductConstr cfg (Arguments cfg more)
         ) => UncurryClient cfg (QueryParams sym (a :: *) :> more) where
  type Arguments  cfg (QueryParams sym a :> more) = [a] : Arguments cfg more
  type Result     cfg (QueryParams sym a :> more) = Result cfg more
  unCurry cfg _ c (uncons cfg (Proxy @(Arguments cfg more)) -> (a, as)) = unCurry cfg (Proxy @more) (c a) as

instance ( HasReflexClient' cfg (prefix $$ Sub (QueryParams sym a)) more
         ) =>
         HasReflexClient' cfg prefix (QueryParams sym a :> more) where
  type BuildReflexClient cfg prefix (QueryParams sym a :> more) =
    BuildReflexClient cfg (prefix $$ Sub (QueryParams sym a)) more
  reflexClient' cfg _ _ = reflexClient' cfg (Proxy @(prefix $$ Sub (QueryParams sym a))) (Proxy @more)


-- ReqBody

#if MIN_VERSION_servant(0,13,0)
instance ( UncurryClient cfg more
         , ProductConstr cfg (Arguments cfg more)
         ) => UncurryClient cfg (ReqBody' mods (ct ': cts) (a :: *) :> more) where
  type Arguments  cfg (ReqBody' mods (ct ': cts) a :> more) = a ': Arguments cfg more
  type Result     cfg (ReqBody' mods (ct ': cts) a :> more) = Result cfg more
#else
instance ( UncurryClient cfg more
         , ProductConstr cfg (Arguments cfg more)
         ) => UncurryClient cfg (ReqBody (ct ': cts) (a :: *) :> more) where
  type Arguments  cfg (ReqBody (ct ': cts) a :> more) = a ': Arguments cfg more
  type Result     cfg (ReqBody (ct ': cts) a :> more) = Result cfg more
#endif
  unCurry cfg _ c (uncons cfg (Proxy @(Arguments cfg more)) -> (a, as)) = unCurry cfg (Proxy @more) (c a) as

#if MIN_VERSION_servant(0,13,0)
instance ( HasReflexClient' cfg (prefix :.: Sub (ReqBody' mods (ct ': cts) a)) more
         ) =>
         HasReflexClient' cfg prefix (ReqBody' mods (ct ': cts) a :> more) where
  type BuildReflexClient cfg prefix (ReqBody' mods (ct ': cts) a :> more) =
    BuildReflexClient cfg (prefix :.: Sub (ReqBody' mods (ct ': cts) a)) more
#else
instance ( HasReflexClient' cfg (prefix :.: Sub (ReqBody' mods (ct ': cts) a)) more
         ) =>
         HasReflexClient' cfg prefix (ReqBody (ct ': cts) a :> more) where
  type BuildReflexClient cfg prefix (ReqBody (ct ': cts) a :> more) =
    BuildReflexClient cfg (prefix :.: Sub (ReqBody (ct ': cts) a)) more
#endif
  reflexClient' cfg _ _ = reflexClient' cfg (Proxy @(prefix :.: Sub (ReqBody' mods (ct ': cts) a))) (Proxy @more)


-- Capture

#if MIN_VERSION_servant(0,13,0)
instance ( UncurryClient cfg more
         , ProductConstr cfg (Arguments cfg more)
         ) => UncurryClient cfg (Capture' mods sym (a :: *) :> more) where
  type Arguments  cfg (Capture' mods sym a :> more) = a ': Arguments cfg more
  type Result     cfg (Capture' mods sym a :> more) = Result cfg more
#else
instance ( UncurryClient cfg more
         , ProductConstr cfg (Arguments cfg more)
         ) => UncurryClient cfg (Capture sym (a :: *) :> more) where
  type Arguments  cfg (Capture sym a :> more) = a ': Arguments cfg more
  type Result     cfg (Capture sym a :> more) = Result cfg more
#endif
  unCurry cfg _ c (uncons cfg (Proxy @(Arguments cfg more)) -> (a, as)) = unCurry cfg (Proxy @more) (c a) as

instance ( HasReflexClient' cfg (prefix :.: Sub (Capture' mods sym a)) more
         ) =>
         HasReflexClient' cfg prefix (Capture' mods sym (a :: *) :> more) where
#if MIN_VERSION_servant(0,13,0)
  type BuildReflexClient cfg prefix (Capture' mods sym a :> more) =
    BuildReflexClient cfg (prefix :.: Sub (Capture' mods sym a)) more
#else
  type BuildReflexClient cfg prefix (Capture sym a :> more) =
    BuildReflexClient cfg (prefix :.: Sub (Capture sym a)) more
#endif
  reflexClient' cfg _ _ = reflexClient' cfg (Proxy @(prefix :.: Sub (Capture' mods sym a))) (Proxy @more)


-- Header'

instance ( UncurryClient cfg more
         , ProductConstr cfg (Arguments cfg more)
         ) => UncurryClient cfg (Header' mods sym (a :: *) :> more) where
#if MIN_VERSION_servant(0,13,0)
  type Arguments  cfg (Header' mods sym a :> more) = RequiredArgument mods a ': Arguments cfg more
  type Result     cfg (Header' mods sym a :> more) = Result cfg more
#else
  type Arguments  cfg (Header sym a :> more) = Maybe a ': Arguments cfg more
  type Result     cfg (Header sym a :> more) = Result cfg more
#endif
  unCurry cfg _ c (uncons cfg (Proxy @(Arguments cfg more)) -> (a, as)) = unCurry cfg (Proxy @more) (c a) as

instance ( HasReflexClient' cfg (prefix :.: Sub (Header' mods sym a)) more
         ) =>
         HasReflexClient' cfg prefix (Header' mods sym a :> more) where
#if MIN_VERSION_servant(0,13,0)
  type BuildReflexClient cfg prefix (Header' mods sym a :> more) =
    BuildReflexClient cfg (prefix :.: Sub (Header' mods sym a)) more
#else
  type BuildReflexClient cfg prefix (Header sym a :> more) =
    BuildReflexClient cfg (prefix :.: Sub (Header sym a)) more
#endif
  reflexClient' cfg _ _ = reflexClient' cfg (Proxy @(prefix :.: Sub (Header' mods sym a))) (Proxy @more)


------------------------------------------------------------------------
-- leaves
--
-- These terminate HasReflexClient and delegate to, most notably,
-- UncurryClient and servant-client-core's HasClient to do the dirty
-- work.


-- Verb

instance UncurryClient cfg (Verb method statusCode contentTypes (a :: *)) where
  type Arguments  cfg (Verb method statusCode contentTypes a) = '[]
  type Result     cfg (Verb method statusCode contentTypes a) = GenericClientM a
  unCurry cfg _ (c) _ = c

instance ( HasClient GenericClientM (prefix $$ Verb method statusCode contentTypes a)
         , UncurryClient cfg (prefix $$ (Verb method statusCode contentTypes a))
         , GenericClientM a ~ Result cfg (prefix $$ Verb method statusCode contentTypes a)
         , EndpointConfig cfg
         , ProductConstr cfg (Arguments cfg (prefix $$ Verb method statusCode contentTypes a))
         ) =>
         HasReflexClient' cfg prefix (Verb method statusCode contentTypes a) where
  type BuildReflexClient cfg prefix (Verb method statusCode contentTypes a) =
    EndpointOf cfg (ProductOf cfg (Arguments cfg (prefix $$ (Verb method statusCode contentTypes a)))) a

  reflexClient' cfg _ _ =
    let
      endpointProxy = Proxy @(prefix $$ (Verb method statusCode contentTypes a))
      cl :: Client GenericClientM (prefix $$ (Verb method statusCode contentTypes a))
      cl = endpointProxy `clientIn` Proxy @GenericClientM
    in mkEndpoint cfg $ unCurry cfg endpointProxy cl


-- Raw

instance ( ProductConstr cfg '[]
         ) => UncurryClient cfg Raw where
  type Arguments  cfg Raw = '[H.Method]
  type Result     cfg Raw = GenericClientM Response
  unCurry cfg _ c (uncons cfg (Proxy @'[]) -> (m, _)) = c m

instance ( HasClient GenericClientM (prefix $$ Raw)
         , UncurryClient cfg (prefix $$ Raw)
         , GenericClientM Response ~ Result cfg (prefix $$ Raw)
         , EndpointConfig cfg
         , ProductConstr cfg (Arguments cfg (prefix $$ Raw))
         ) =>
         HasReflexClient' cfg prefix Raw where
  type BuildReflexClient cfg prefix Raw =
    EndpointOf cfg (ProductOf cfg (Arguments cfg (prefix $$ Raw))) Response

  reflexClient' cfg _ _ =
    let
      endpointProxy = Proxy @(prefix $$ Raw)
      cl :: Client GenericClientM (prefix $$ Raw)
      cl = endpointProxy `clientIn` Proxy @GenericClientM
    in mkEndpoint cfg $ unCurry cfg endpointProxy cl

#if !MIN_VERSION_servant(0,13,0)
type Header' mods = Header
type Capture' mods = Capture
type ReqBody' mods = ReqBody
#endif
