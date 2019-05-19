module Action.Dice
  ( execute
  ) where

import Prelude

import Bouzuya.HTTP.Client as Client
import Bouzuya.HTTP.Method as Method
import Data.Array.NonEmpty as NonEmptyArray
import Data.Int as Int
import Data.Maybe (Maybe)
import Data.Maybe as Maybe
import Data.Options ((:=))
import Data.String.Regex (Regex)
import Data.String.Regex as Regex
import Data.String.Regex.Flags as RegexFlags
import Data.String.Regex.Unsafe as RegexUnsafe
import Data.Tuple (Tuple)
import Data.Tuple as Tuple
import Effect (Effect)
import Effect as Effect
import Effect.Class as Class
import Effect.Random as Random
import Effect.Ref as Ref
import Foreign.Object as Object
import HTTPure as HTTPure
import View.Dice as ViewDice

type Dice = Tuple Int Int

regex :: Regex
regex = RegexUnsafe.unsafeRegex "^(\\d+)d(\\d+)$" RegexFlags.noFlags

parseDice :: String -> Maybe Dice
parseDice s = do
  matches <- Regex.match regex s
  a <- (join (NonEmptyArray.index matches 1)) >>= Int.fromString
  x <- (join (NonEmptyArray.index matches 2)) >>= Int.fromString
  if a < 1 then Maybe.Nothing else pure unit
  if x < 1 then Maybe.Nothing else pure unit
  pure (Tuple.Tuple a x)

rollDice :: Dice -> Effect Int
rollDice (Tuple.Tuple a x) = do
  countRef <- Ref.new 0
  Effect.forE 0 a \_ -> do
    count <- Random.randomInt 1 x
    void (Ref.modify (add count) countRef)
  Ref.read countRef

showDice :: Dice -> String
showDice (Tuple.Tuple a x) = show a <> "d" <> show x

execute :: Maybe String -> String -> HTTPure.ResponseM
execute urlMaybe s = do
  case parseDice s of
    Maybe.Nothing -> HTTPure.badRequest ("invalid dice: " <> s)
    Maybe.Just dice -> do
      n <- Class.liftEffect (rollDice dice)
      let
        headers = HTTPure.header "Content-Type" "application/json"
        rendered = ViewDice.render (showDice dice) n
      case urlMaybe of
        Maybe.Just url -> do
          void
            (Client.fetch
              ( Client.url := url
              <> Client.headers :=
                  (Object.fromFoldable
                    [ Tuple.Tuple "Content-Type" "application/json"
                    ])
              <> Client.method := Method.POST
              <> Client.body := rendered))
        Maybe.Nothing -> pure unit
      HTTPure.ok' headers rendered
