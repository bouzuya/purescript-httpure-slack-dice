module Main
  ( main
  ) where

import Prelude

import Action as Action
import Data.Either as Either
import Data.Int as Int
import Data.Maybe as Maybe
import Effect (Effect)
import Effect.Class as Class
import Effect.Console as Console
import Effect.Exception as Exception
import HTTPure (Request, ResponseM)
import HTTPure as HTTPure
import Node.HTTP (ListenOptions)
import Node.Process as Process
import Router as Router

readPort :: Effect Int
readPort = do
  portStringMaybe <- Process.lookupEnv "PORT"
  let portMaybe = Maybe.maybe (pure 8080) Int.fromString portStringMaybe
  Maybe.maybe (Exception.throw "invalid port") pure portMaybe

main :: Effect Unit
main = do
  port <- readPort
  _ <-
    HTTPure.serve'
      (listenOptions port)
      app
      (Console.log ("Server now up on port " <> show port))
  pure unit
  where
    app :: Request -> ResponseM
    app request =
      case Router.router request of
        Either.Right action -> Action.execute action
        Either.Left (Router.ClientError e) -> do
          Class.liftEffect (Console.log e)
          HTTPure.badRequest "invalid params"
        Either.Left Router.NotFound ->
          HTTPure.notFound

    listenOptions :: Int -> ListenOptions
    listenOptions port =
      { backlog: Maybe.Nothing
      , hostname: "0.0.0.0"
      , port
      }
