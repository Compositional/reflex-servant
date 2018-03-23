{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Reflex.Servant.Internal
  ( module Reflex.Servant.Internal
  , module Reflex.Servant.Internal.GenericClientM
  ) where

import Data.Proxy
import GHC.TypeLits
import Servant.API
import Servant.API.Modifiers
import Servant.Client.Core
import Reflex.Servant.Internal.GenericClientM
import Reflex.Servant.Internal.TypeFunction

import qualified Network.HTTP.Types                     as H

-- | A finished reflex client consisting of @:<|>@ and @Endpoint i o@
-- for various @i@ and @o@.
type ReflexClient api = BuildReflexClient I api

class HasReflexClient' I api => HasReflexClient api
instance HasReflexClient' I api => HasReflexClient api

reflexClient :: HasReflexClient api => Proxy api -> ReflexClient api
reflexClient api = reflexClient' (Proxy @I) api

-- prefix:  single-endpoint api builder
-- api:     api (pattern matching)
class HasReflexClient' (prefix :: *) (api :: *) where
  type BuildReflexClient prefix api :: *
  reflexClient' :: Proxy prefix -> Proxy api -> BuildReflexClient prefix api

-- | Type level @Sub a = \b -> a :> b@
type Sub a = Lift ((:>) a)

-- | @forall m. 'RunClient' m => i -> m o@. A computation that can be run in
-- various @servant-client@ implementations' @ClientM@ monadic types.
newtype Endpoint i o = Endpoint { runEndpoint :: i -> GenericClientM o }

class UncurryClient (api :: *) where
  type Arguments api :: *
  type Result api :: *
  unCurry :: Proxy api -> Client GenericClientM api -> Arguments api -> Result api


-- Fish

instance ( HasReflexClient' prefix a
         , HasReflexClient' prefix b
         ) =>
         HasReflexClient' prefix (a :<|> b) where
  type BuildReflexClient prefix (a :<|> b) = BuildReflexClient prefix a :<|> BuildReflexClient prefix b

  reflexClient' p _ = reflexClient' p (Proxy @a) :<|> reflexClient' p (Proxy @b)


-- Sub (aka "path" :> ...)

instance UncurryClient more => UncurryClient ((sym :: Symbol) :> more) where
  type Arguments  (sym :> more) = Arguments more
  type Result     (sym :> more) = Result more
  unCurry _ c a = unCurry (Proxy @more) c a

instance forall prefix (sym :: Symbol) more.
         HasReflexClient' (prefix :.: Sub sym) more =>
         HasReflexClient' prefix (sym :> more) where
  type BuildReflexClient prefix (sym :> more) =
       BuildReflexClient (prefix :.: Sub sym) more
  reflexClient' _ _ = reflexClient' (Proxy @(prefix :.: Sub sym)) (Proxy @more)


-- Description

instance UncurryClient more => UncurryClient (Description sym :> more) where
  type Arguments  (Description sym :> more) = Arguments more
  type Result     (Description sym :> more) = Result more
  unCurry _ c a = unCurry (Proxy @more) c a



-- QueryParams

instance UncurryClient more => UncurryClient (QueryParams sym a :> more) where
  type Arguments  (QueryParams sym a :> more) = [a] :<|> Arguments more
  type Result     (QueryParams sym a :> more) = Result more
  unCurry _ c (a :<|> as) = unCurry (Proxy @more) (c a) as

instance ( HasReflexClient' (prefix $$ Sub (QueryParams sym a)) more
         ) =>
         HasReflexClient' prefix (QueryParams sym a :> more) where
  type BuildReflexClient prefix (QueryParams sym a :> more) =
    BuildReflexClient (prefix $$ Sub (QueryParams sym a)) more
  reflexClient' _ _ = reflexClient' (Proxy @(prefix $$ Sub (QueryParams sym a))) (Proxy @more)


-- ReqBody

instance UncurryClient more => UncurryClient (ReqBody' mods (ct ': cts) a :> more) where
  type Arguments  (ReqBody' mods (ct ': cts) a :> more) = a :<|> Arguments more
  type Result     (ReqBody' mods (ct ': cts) a :> more) = Result more
  unCurry _ c (a :<|> as) = unCurry (Proxy @more) (c a) as

instance ( HasReflexClient' (prefix :.: Sub (ReqBody' mods (ct ': cts) a)) more
         ) =>
         HasReflexClient' prefix (ReqBody' mods (ct ': cts) a :> more) where
  type BuildReflexClient prefix (ReqBody' mods (ct ': cts) a :> more) =
    BuildReflexClient (prefix :.: Sub (ReqBody' mods (ct ': cts) a)) more
  reflexClient' _ _ = reflexClient' (Proxy @(prefix :.: Sub (ReqBody' mods (ct ': cts) a))) (Proxy @more)


-- Capture

instance UncurryClient more => UncurryClient (Capture' mods sym a :> more) where
  type Arguments  (Capture' mods sym a :> more) = a :<|> Arguments more
  type Result     (Capture' mods sym a :> more) = Result more
  unCurry _ c (a :<|> as) = unCurry (Proxy @more) (c a) as

instance ( HasReflexClient' (prefix :.: Sub (Capture' mods sym a)) more
         ) =>
         HasReflexClient' prefix (Capture' mods sym a :> more) where
  type BuildReflexClient prefix (Capture' mods sym a :> more) =
    BuildReflexClient (prefix :.: Sub (Capture' mods sym a)) more
  reflexClient' _ _ = reflexClient' (Proxy @(prefix :.: Sub (Capture' mods sym a))) (Proxy @more)


-- Header'

instance UncurryClient more => UncurryClient (Header' mods sym (a :: *) :> more) where
  type Arguments  (Header' mods sym a :> more) = RequiredArgument mods a :<|> Arguments more
  type Result     (Header' mods sym a :> more) = Result more
  unCurry _ c (a :<|> as) = unCurry (Proxy @more) (c a) as

instance ( HasReflexClient' (prefix :.: Sub (Header' mods sym a)) more
         ) =>
         HasReflexClient' prefix (Header' mods sym a :> more) where
  type BuildReflexClient prefix (Header' mods sym a :> more) =
    BuildReflexClient (prefix :.: Sub (Header' mods sym a)) more
  reflexClient' _ _ = reflexClient' (Proxy @(prefix :.: Sub (Header' mods sym a))) (Proxy @more)


-- Verb

instance UncurryClient (Verb method statusCode contentTypes a) where
  type Arguments  (Verb method statusCode contentTypes a) = ()
  type Result     (Verb method statusCode contentTypes a) = GenericClientM a
  unCurry _ c _ = c

instance ( HasClient (GenericClientM) (prefix $$ (Verb method statusCode contentTypes a))
         , UncurryClient (prefix $$ (Verb method statusCode contentTypes a))
         , GenericClientM a ~ Result (prefix $$ (Verb method statusCode contentTypes a))
         ) =>
         HasReflexClient' prefix (Verb method statusCode contentTypes a) where
  type BuildReflexClient prefix (Verb method statusCode contentTypes a) =
    Endpoint (Arguments (prefix $$ (Verb method statusCode contentTypes a))) a

  reflexClient' _ _ =
    let
      endpointProxy = Proxy @(prefix $$ (Verb method statusCode contentTypes a))
      cl :: Client GenericClientM (prefix $$ (Verb method statusCode contentTypes a))
      cl = endpointProxy `clientIn` Proxy @GenericClientM
    in Endpoint $ unCurry endpointProxy cl


-- Raw

instance UncurryClient Raw where
  type Arguments  Raw = H.Method
  type Result     Raw = GenericClientM Response
  unCurry _ c m = c m

instance ( HasClient (GenericClientM) (prefix $$ Raw)
         , UncurryClient (prefix $$ Raw)
         , GenericClientM Response ~ Result (prefix $$ Raw)
         ) =>
         HasReflexClient' prefix Raw where
  type BuildReflexClient prefix Raw =
    Endpoint (Arguments (prefix $$ Raw)) Response

  reflexClient' _ _ =
    let
      endpointProxy = Proxy @(prefix $$ Raw)
      cl :: Client GenericClientM (prefix $$ Raw)
      cl = endpointProxy `clientIn` Proxy @GenericClientM
    in Endpoint $ unCurry endpointProxy cl
