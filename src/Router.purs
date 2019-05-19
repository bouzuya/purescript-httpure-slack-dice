module Router
  ( RouteError(..)
  , router
  ) where

import Prelude

import Action (Action(..))
import Data.Array as Array
import Data.Either (Either)
import Data.Either as Either
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Foreign as Foreign
import HTTPure as HTTPure
import Node.URL as URL
import Simple.JSON (class ReadForeign)
import Simple.JSON as SimpleJSON

data RouteError
  = ClientError String
  | NotFound

derive instance eqRouteError :: Eq RouteError
derive instance genericRouteError :: Generic RouteError _
instance showRouteError :: Show RouteError where
  show = genericShow

fromJSON :: forall a. ReadForeign a => String -> Either RouteError a
fromJSON s =
  Either.either
    (Either.Left <<< ClientError <<< show)
    Either.Right
    (SimpleJSON.readJSON s)

fromURLEncoded :: forall a. ReadForeign a => String -> Either RouteError a
fromURLEncoded s =
  Either.either
    (Either.Left <<< ClientError <<< show)
    Either.Right
    (SimpleJSON.read (Foreign.unsafeToForeign (URL.parseQueryString s)))

type Payload =
  { token :: String -- DEPRECATED
  , command :: String
  , text :: String
  -- ...
  }

type InteractPayload =
  { payload ::
    { actions ::
        Array
        { action_id :: String
        , value :: String
        -- ...
        }
    -- ...
    }
  }

router :: HTTPure.Request -> Either RouteError Action
router request =
  case request.path of
    ["action"] ->
      case request.method of
        HTTPure.Post -> do
          payload <- fromJSON request.body :: _ _ InteractPayload
          action <-
            Either.note
              (ClientError "no action")
              (Array.head payload.payload.actions)
          pure (Dice action.value)
        _ -> Either.Left NotFound -- TODO: 405
    ["dice"] ->
      case request.method of
        HTTPure.Post -> do
          payload <- fromURLEncoded request.body :: _ _ Payload
          pure (Dice payload.text)
        _ -> Either.Left NotFound -- TODO: 405
    _ -> Either.Left NotFound
