{-# LANGUAGE ConstraintKinds #-}
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
  ) where

import Data.Proxy
import GHC.TypeLits
import GHC.Exts(Constraint)
import Servant.API
import Servant.API.Modifiers
import Servant.Client.Core
import Reflex.Servant.Internal.GenericClientM
import Reflex.Servant.Internal.TypeFunction
import Reflex.Servant.Internal.Product


import qualified Network.HTTP.Types                     as H

-- | A finished reflex client consisting of @:<|>@ and @Endpoint i o@
-- for various @i@ and @o@.
type ReflexClient config api = BuildReflexClient config I api

class HasReflexClient' config I api => HasReflexClient config api
instance HasReflexClient' config I api => HasReflexClient config api

------------------------------------------------------------------------

class EndpointConfig ec where
  type EndpointOf ec i o :: *
  mkEndpoint :: ec -> (i -> GenericClientM o) -> EndpointOf ec i o

------------------------------------------------------------------------

-- TODO: replace by something like servant-server's context?
data Config p e = Config
  { configProduct :: p
  , configEndpoint :: e
  }

instance ProductConfig p => ProductConfig (Config p _e) where
  type ProductOf (Config p _e) ts = ProductOf p ts
  type ProductConstraint (Config p _e) = ProductConstraint p
  productNil c = productNil $ configProduct c
  productCons c = productCons $ configProduct c
  productHead c = productHead $ configProduct c
  productTail c = productTail $ configProduct c

instance EndpointConfig ec => EndpointConfig (Config _p ec) where
  type EndpointOf (Config _p ec) i o = EndpointOf ec i o
  mkEndpoint c = mkEndpoint $ configEndpoint c

------------------------------------------------------------------------

-- | 'EndpointConfig' that provides extra configuration to its the runner
data ConfiguredEndpointConfig c = ConfiguredEndpointConfig c

-- | An endpoint to be run by an implementation of @servant-client-core@ using
-- configuration @c@.
newtype Endpoint c i o = Endpoint { runEndpoint :: i -> (c, GenericClientM o)
                                  }

instance EndpointConfig (ConfiguredEndpointConfig c) where
  type EndpointOf (ConfiguredEndpointConfig c) i o = Endpoint c i o
  mkEndpoint (ConfiguredEndpointConfig c) = Endpoint . ((c,).)

------------------------------------------------------------------------

reflexClient :: HasReflexClient config api => config -> Proxy api -> ReflexClient config api
reflexClient config api = reflexClient' config (Proxy @I) api

-- prefix:  single-endpoint api builder
-- api:     api (pattern matching)
class HasReflexClient' (config :: *) (prefix :: *) (api :: *) where
  type BuildReflexClient config prefix api :: *
  reflexClient' :: config -> Proxy prefix -> Proxy api -> BuildReflexClient config prefix api

-- | Type level @Sub a = (a :>) = \b -> a :> b@
type Sub a = Lift ((:>) a)

class UncurryClient config (api :: *) where
  type Arguments config api :: *
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
         ) => UncurryClient cfg (QueryParams sym a :> more) where
  type Arguments  cfg (QueryParams sym a :> more) = Cons [a] (Arguments cfg more)
  type Result     cfg (QueryParams sym a :> more) = Result cfg more
  unCurry cfg _ c (uncons cfg (Proxy @(Arguments cfg more)) -> (a, as)) = unCurry cfg (Proxy @more) (c a) as

instance ( HasReflexClient' cfg (prefix $$ Sub (QueryParams sym a)) more
         ) =>
         HasReflexClient' cfg prefix (QueryParams sym a :> more) where
  type BuildReflexClient cfg prefix (QueryParams sym a :> more) =
    BuildReflexClient cfg (prefix $$ Sub (QueryParams sym a)) more
  reflexClient' cfg _ _ = reflexClient' cfg (Proxy @(prefix $$ Sub (QueryParams sym a))) (Proxy @more)


-- ReqBody

instance ( UncurryClient cfg more
         , ProductConstr cfg (Arguments cfg more)
         ) => UncurryClient cfg (ReqBody' mods (ct ': cts) a :> more) where
  type Arguments  cfg (ReqBody' mods (ct ': cts) a :> more) = Cons a (Arguments cfg more)
  type Result     cfg (ReqBody' mods (ct ': cts) a :> more) = Result cfg more
  unCurry cfg _ c (uncons cfg (Proxy @(Arguments cfg more)) -> (a, as)) = unCurry cfg (Proxy @more) (c a) as

instance ( HasReflexClient' cfg (prefix :.: Sub (ReqBody' mods (ct ': cts) a)) more
         ) =>
         HasReflexClient' cfg prefix (ReqBody' mods (ct ': cts) a :> more) where
  type BuildReflexClient cfg prefix (ReqBody' mods (ct ': cts) a :> more) =
    BuildReflexClient cfg (prefix :.: Sub (ReqBody' mods (ct ': cts) a)) more
  reflexClient' cfg _ _ = reflexClient' cfg (Proxy @(prefix :.: Sub (ReqBody' mods (ct ': cts) a))) (Proxy @more)


-- Capture

instance ( UncurryClient cfg more
         , ProductConstr cfg (Arguments cfg more)
         ) => UncurryClient cfg (Capture' mods sym a :> more) where
  type Arguments  cfg (Capture' mods sym a :> more) = Cons a (Arguments cfg more)
  type Result     cfg (Capture' mods sym a :> more) = Result cfg more
  unCurry cfg _ c (uncons cfg (Proxy @(Arguments cfg more)) -> (a, as)) = unCurry cfg (Proxy @more) (c a) as

instance ( HasReflexClient' cfg (prefix :.: Sub (Capture' mods sym a)) more
         ) =>
         HasReflexClient' cfg prefix (Capture' mods sym a :> more) where
  type BuildReflexClient cfg prefix (Capture' mods sym a :> more) =
    BuildReflexClient cfg (prefix :.: Sub (Capture' mods sym a)) more
  reflexClient' cfg _ _ = reflexClient' cfg (Proxy @(prefix :.: Sub (Capture' mods sym a))) (Proxy @more)


-- Header'

instance ( UncurryClient cfg more
         , ProductConstr cfg (Arguments cfg more)
         ) => UncurryClient cfg (Header' mods sym (a :: *) :> more) where
  type Arguments  cfg (Header' mods sym a :> more) = Cons (RequiredArgument mods a) (Arguments cfg more)
  type Result     cfg (Header' mods sym a :> more) = Result cfg more
  unCurry cfg _ c (uncons cfg (Proxy @(Arguments cfg more)) -> (a, as)) = unCurry cfg (Proxy @more) (c a) as

instance ( HasReflexClient' cfg (prefix :.: Sub (Header' mods sym a)) more
         ) =>
         HasReflexClient' cfg prefix (Header' mods sym a :> more) where
  type BuildReflexClient cfg prefix (Header' mods sym a :> more) =
    BuildReflexClient cfg (prefix :.: Sub (Header' mods sym a)) more
  reflexClient' cfg _ _ = reflexClient' cfg (Proxy @(prefix :.: Sub (Header' mods sym a))) (Proxy @more)


-- Verb

instance UncurryClient cfg (Verb method statusCode contentTypes a) where
  type Arguments  cfg (Verb method statusCode contentTypes a) = Nil
  type Result     cfg (Verb method statusCode contentTypes a) = GenericClientM a
  unCurry cfg _ c _ = c

instance ( HasClient (GenericClientM) (prefix $$ (Verb method statusCode contentTypes a))
         , UncurryClient cfg (prefix $$ (Verb method statusCode contentTypes a))
         , GenericClientM a ~ Result cfg (prefix $$ (Verb method statusCode contentTypes a))
         , ProductConstr cfg (Arguments cfg (prefix $$ Verb method statusCode contentTypes a))
         , EndpointConfig cfg
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

instance ( ProductConfig cfg
         , ProductConstraint cfg Nil
         ) => UncurryClient cfg Raw where
  type Arguments  cfg Raw = Cons H.Method Nil
  type Result     cfg Raw = GenericClientM Response
  unCurry cfg _ c (uncons cfg (Proxy @Nil) -> (m, _)) = c m -- (productCons cfg (Proxy @Nil) m (productNil cfg))

instance ( HasClient (GenericClientM) (prefix $$ Raw)
         , UncurryClient cfg (prefix $$ Raw)
         , GenericClientM Response ~ Result cfg (prefix $$ Raw)
         , ProductConstr cfg (Arguments cfg (prefix $$ Raw))
         , EndpointConfig cfg
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
