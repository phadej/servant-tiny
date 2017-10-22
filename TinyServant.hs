{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module TinyServant where

import Control.Applicative
import Control.Monad (join)
import Data.Time
import GHC.TypeLits
import Text.Read
import Data.Proxy (Proxy (..))

-------------------------------------------------------------------------------
-- API specification DSL
-------------------------------------------------------------------------------

-- $apidsl
--
-- The idea is that
--
-- @
-- a :> b :> Verb (c :< d)
-- @
--
-- is transformed into
--
-- @
-- a -> b -> m (c, (d, x))
-- @
--
-- where (not so obviously) not only @->@ or @(,)@ may be applied.
--
-- So loosely:
--
-- * @:>@ is transformed into @->@,
--
-- * @:+@ is tranformed into product @(,)@ (or sum!),
--
-- * @Verb@ is a point where LHS and RHS meet, i.e. some monad @m@.

data Verb (a :: *)
data Full (a :: *)
-- I'm not keen of this name, for streaming response it will be simply Stream;
-- but what are non-stream response?
--
-- Also NoContent can be own response type.

data a :<|> b = a :<|> b
infixr 8 :<|>

data (a :: k) :> (b :: *)
infixr 9 :>

-- Q: Should we just reuse :> ?
data (a :: l) :< (b :: *)
infixr 9 :<

data Capture (a :: *)

-------------------------------------------------------------------------------
-- Example API
-------------------------------------------------------------------------------

-- | Day is always known, but for some timezones we might not know the time!
--
-- The key difference is that we declare which endpoints can fail.
-- 'ServantErr' here is "unchecked exception", but it's possible to write
-- checked exception variant.
--
type MyAPI = "date" :> Verb (Full Day)
        :<|> "time" :> Capture TimeZone :> Verb (ServantErr :< Full ZonedTime)

-- | Type of errors.
newtype ServantErr = ServantErr String

-------------------------------------------------------------------------------
-- Handlers
-------------------------------------------------------------------------------

handleDate :: IO Day
handleDate = utctDay <$> getCurrentTime

handleTime :: TimeZone -> IO (Either ServantErr ZonedTime)
handleTime tz
    | timeZoneMinutes tz < 0 = pure $ Left $ ServantErr
        "We don't support timezones with negative offsets"
    | otherwise             = Right . utcToZonedTime tz <$> getCurrentTime

handleMyAPI :: Server MyAPI
handleMyAPI = handleDate :<|> handleTime

-------------------------------------------------------------------------------
-- Server + HasServer
-------------------------------------------------------------------------------

-- | The 'HasServer' class
class HasServer layout where
  type Server layout :: *
  route :: Proxy layout -> Server layout -> [String] -> Either String (IO String)

instance (HasServer a, HasServer b) => HasServer (a :<|> b) where
  type Server (a :<|> b) = Server a :<|> Server b

  route :: Proxy (a :<|> b) -> (Server a :<|> Server b) -> [String] -> Either String (IO String)
  route _ (handlera :<|> handlerb) xs = pickRight
    (route (Proxy :: Proxy a) handlera xs)
    (route (Proxy :: Proxy b) handlerb xs)
    where
      pickRight (Right x) _ = Right x
      pickRight (Left _)  y = y

instance (KnownSymbol s, HasServer r) => HasServer (s :> r) where
  type Server (s :> r) = Server r

  route :: Proxy (s :> r) -> Server r -> [String] -> Either String (IO String)
  route _ handler (x : xs)
    | symbolVal (Proxy :: Proxy s) == x = route (Proxy :: Proxy r) handler xs
  route _ _       _                     = Left "no match"

instance (Read a, HasServer r) => HasServer (Capture a :> r) where
  type Server (Capture a :> r) = a -> Server r

  route :: Proxy (Capture a :> r) -> (a -> Server r) -> [String] -> Either String (IO String)
  route _ handler (x : xs) = do
    a <- maybe (Left "no parse") Right $ readMaybe x
    route (Proxy :: Proxy r) (handler a) xs
  route _ _       _        = Left "no match"

instance (HasServer' a) => HasServer (Verb a) where
  type Server (Verb a) = IO (Server' a)

  route :: Proxy (Verb a) -> IO (Server' a) -> [String] -> Either String (IO String)
  route _ handler [] = Right $ handler >>= route' (Proxy :: Proxy a)
  route _ _       _  = Left "no match"

class HasServer' a where
  type Server' a :: *
  route' :: Proxy a -> Server' a -> IO String

instance HasServer' a => HasServer' (ServantErr :< a) where
  type Server' (ServantErr :< a) = Either ServantErr (Server' a)

  route' :: Proxy (ServantErr :< a) -> Either ServantErr (Server' a) -> IO String
  route' _ (Left (ServantErr err)) = pure $ "HANDLER-ERROR: " ++ err
  route' _ (Right x)        = route' (Proxy :: Proxy a) x

instance Show a => HasServer' (Full a) where
  type Server' (Full a) = a

  route' :: Proxy (Full a) -> a -> IO String
  route' _ x = pure (show x)

  -- verb :: Show a => Proxy a -> Proxy b -> IO (ServerTrans b a) -> Either String (IO String)

-------------------------------------------------------------------------------
-- Serve
-------------------------------------------------------------------------------

serve :: HasServer layout
      => Proxy layout -> Server layout -> [String] -> IO String
serve p h xs = case route p h xs of
  Left err -> fail $ "ERROR: " ++ err
  Right x  -> x

{-

From blog post: Works as previously:

*TinyServant> serve (Proxy :: Proxy MyAPI) handleMyAPI ["time", "CET"]
"2017-10-22 17:14:59.382719319 CET"
*TinyServant> serve (Proxy :: Proxy MyAPI) handleMyAPI ["time", "12"]
*** Exception: user error (ERROR: no parse)
*TinyServant> serve (Proxy :: Proxy MyAPI) handleMyAPI ["date"]
"2017-10-22"
*TinyServant> serve (Proxy :: Proxy MyAPI) handleMyAPI []
*** Exception: user error (ERROR: no match)

*TinyServant> serve (Proxy :: Proxy MyAPI) handleMyAPI ["time", "-01:00"]
"HANDLER-ERROR: We don't support timezones with negative offsets"
-}

-------------------------------------------------------------------------------
-- Bonus: No overlapping headers
-------------------------------------------------------------------------------

data Header (name :: Symbol) b

instance (KnownSymbol name, Show b, HasServer' a) => HasServer' (Header name b :< a) where
  type Server' (Header name b :< a) = (b, Server' a)

  route' :: Proxy (Header name b :< a) -> (b, Server' a) -> IO String
  route' _ (value, x) = do
      x' <- route' (Proxy :: Proxy a) x
      pure $ symbolVal (Proxy :: Proxy name) ++ ": " ++ show value ++ "; " ++ x'

type HeaderAPI =
  "time" :> Capture TimeZone :> Verb (Header "X-Tz" TimeZone :< ServantErr :< Full ZonedTime)

-- |
--
-- @
-- *TinyServant> :kind! Server HeaderAPI
-- Server HeaderAPI :: *
-- = TimeZone -> IO (String, Either ServantErr ZonedTime)
-- @
--
-- Compare with 'handleTime'
handleHeaderAPI :: Server HeaderAPI
handleHeaderAPI tz
    | timeZoneMinutes tz < 0 = pure
        (tz, Left $ ServantErr "We don't support timezones with negative offsets")
    | otherwise             = do
        t <- getCurrentTime
        pure (tz, Right (utcToZonedTime tz t))

{-

*TinyServant> serve (Proxy :: Proxy HeaderAPI) handleHeaderAPI  ["time", "-01:00"]
"X-Tz: -0100; HANDLER-ERROR: We don't support timezones with negative offsets"

-}
