
`reflex-servant` lets you access [Servant](https://haskell-servant.readthedocs.io/en/stable/) APIs with [Reflex FRP](http://docs.reflex-frp.org/en/latest/).

 * API calls are as simple as `Event in -> m (Event out)`.
 * Minimal dependencies, covered by just `reflex` and `servant-client-core`.
 * Customizable product types
 * `traverseEndpoint` for labeled events or sequential calls

# Quick Start

Let's start with some pragma's and imports.

```haskell
{-# LANGUAGE ConstraintKinds, DataKinds, FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings, RankNTypes, ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications, TypeOperators #-}
import Control.Monad
import Control.Monad.IO.Class
import Data.Proxy
import Data.Text(Text)
import Reflex
import Reflex.Servant
import Servant.API
import Servant.Client.Core
```

... and an example API:

```haskell
type Api = "ping" :> Get '[JSON] Text
      :<|> "calendar" :> Capture "calendarName" Text :> "items" :> Get '[JSON] [(Timestamp, Text)]
      :<|> "calendar" :> Capture "calendarName" Text :> "items" :> ReqBody '[JSON] (Timestamp, Text) :> Post '[JSON] [(Timestamp, Text)]

-- not important
type Timestamp = Int
```

Now let's build a simple network. We'll pretend to use `reflex-dom`.

```haskell

myApp :: forall t m. MonadWidget t m => m ()
myApp = do
  postBuild <- getPostBuild
  let ping :<|> list :<|> post = reflexClient (basicConfig myRunner) (Proxy @Api)
  calendarResponse <- list ("Birthday calendar" <$ postBuild)
  calendar <- holdDyn [] (filterRight calendarResponse)
  showCalendar calendar
  
showCalendar :: MonadWidget t m => Dynamic t [(Timestamp, Text)] -> m ()
showCalendar = void . dyn . fmap (undefined :: [(Timestamp, Text)] -> m ())
```

The runner is provided by a library, such as [`servant-client`](https://hackage.haskell.org/package/servant-client) or [`servant-client-jsaddle`](https://github.com/Compositional/servant-client-jsaddle).

Here's what the runner types look like.

```haskell
myRunner :: ServantClientRunner () m
myRunner cfg (GenericClientM m) = myRunServantClient cfg m

myRunServantClient :: () -> (RunClient m' => m' a) -> m (Either ServantError a)
myRunServantClient cfg m = error "Call servant-client or servant-client-jsaddle"
```

The redundant `cfg :: ()` is useful when calling multiple services. You can ignore it for now.


# Advanced use

While the above approach is easy, it is not quite as flexible. If you want to use your client functions in a transformed monad, or if you're not comfortable passing the client functions as arguments, you can use a different configuration.

```haskell
pingCalendar :<|> listCalendar :<|> postCalendar =
  reflexClient (calendarConfig) (Proxy @Api)

calendarConfig = (defaultConfig myRunner)
  { configEndpoint = ConfiguredEndpointConfig ()
  }

myApp' :: forall t m. MonadWidget t m => m ()
myApp' = do
  postBuild <- getPostBuild
  calendarResponse <- endpoint' listCalendar ((,) "Birthday calendar" <$> postBuild)
  calendar <- holdDyn [] (filterRight calendarResponse)
  showCalendar calendar

endpoint'
    :: PerformEvent t m
    => Endpoint () i o
    -> Event t i
    -> m (Event t (Either ServantError o))
endpoint' = endpoint myRunner
```

If your application needs to access multiple APIs, you can make use of a configuration argument to distinguish between APIs.

```haskell
data Service = CalendarService | OtherService

pingCalendar' :<|> listCalendar' :<|> postCalendar' =
  reflexClient (calendarConfig') (Proxy @Api)

calendarConfig' = (defaultConfig myRunner)
  { configEndpoint = ConfiguredEndpointConfig CalendarService
  }

endpoint''
    :: PerformEvent t m
    => Endpoint Service i o
    -> Event t i
    -> m (Event t (Either ServantError o))
endpoint'' = endpoint multiRunner

multiRunner
    :: Service
    -> GenericClientM a
    -> m (Either ServantError a)
multiRunner CalendarService = myRunner ()
multiRunner OtherService = error "Other service runner not configured yet."
```

Of course variations are possible. Instead of a closed `Service` type, you may want to pass something more flexible, such as the runner itself, or you can use an open union if you want to substitute different runners for testing.

# Literate Haskell

This is a [literate readme](https://github.com/sol/markdown-unlit), so you can actually typecheck and run this document!

```haskell
main = pure ()
```

# Host-independent

To illustrate the use of `reflex-servant` we have pretended to use `reflex-dom`, but actually this library does not depend on any particular reflex host. These are the fake definitions use to typecheck this document.

```haskell
type MonadWidget t m = (Reflex t, MonadHold t m, MonadSample t (Performable m), PerformEvent t m, MonadIO (Performable m), PostBuild t m)

dyn :: MonadWidget t m => Dynamic t (m a) -> m (Event t a)
dyn = undefined

simpleTextInput :: MonadWidget t m => m (Dynamic t Text)
simpleTextInput = undefined

datePicker :: MonadWidget t m => m (Dynamic t Timestamp)
datePicker = undefined
```
